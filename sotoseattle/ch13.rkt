#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require racket/trace)

; we start by writing (intersect set1 aset)
; for which we need member? from the last chapter

(define member?
  (lambda (a lat)
    (letrec
        ((∈? (lambda (l)
                      (cond
                        [(null? l) #f]
                        [(eq? a (car l)) #t]
                        [else (∈? (cdr l))]))))
      (∈? lat))))


(define intersect-v1
  (lambda (s1 s2)
    (cond
      [(null? s1) s1]
      [(member? (car s1) s2) (cons (car s1) (intersect-v1 (cdr s1) s2))]
      [else (intersect-v1 (cdr s1) s2)])))

; now let's apply the 12th Commandment: Use letrec to remove arguments that
; don't change in recursion, in this case s2

(define intersect-v2
  (lambda (s1 s2)
    (letrec
      ((∩ (lambda (set)
          (cond
            [(null? set) set]
            [(member? (car set) s2) (cons (car set) (∩ (cdr set)))]
            [else (∩ (cdr set))]))))
      (∩ s1))))

(module+ test
  [check-equal? (intersect-v1 '(1 2 3 4) '(5 6 2 7 3)) '(2 3)]
  [check-equal? (intersect-v2 '(1 2 3 4) '(5 6 2 7 3)) '(2 3)])

; write (intersectall list_of_sets

(define intersectall-v1
  (lambda (lset)
    (cond
      [(null? (cdr lset)) (car lset)]
      [else (intersect-v2 (car lset) (intersectall-v1 (cdr lset)))])))

; letreccify to avoid assuming that lset is not empty

(define intersectall-v2
  (lambda (list-of-sets)
    (letrec
      ((∩∩ (lambda (lset)
            (cond
              [(null? (cdr lset)) (car lset)]
              [else (intersect-v2 (car lset) (intersectall-v2 (cdr lset)))]))))
      (cond
        [(null? list-of-sets) '()]
        [else (∩∩ list-of-sets)]))))

; interestingly enough we could have use perfectly well the name intersectall instead of ∩∩ !!
; yes (page 39) "because (letrec...) hides definitions, and the names matter only inside (letrec...)
; very importan point !!!! <========

(module+ test
  [check-equal? (intersectall-v2 '((1 2 3) (2 3 4) (1 4 5 2))) '(2)]
  [check-equal? (intersectall-v2 '()) '()]
  [check-equal? (intersectall-v2 '((3 mangos and) (3 kiwis and) (3 hamburgers))) '(3)]
  [check-equal? (intersectall-v2 '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))) '()]
  [check-equal? (intersectall-v2 '((3 mangoes and) () (3 diet hamburgers))) '()]
)

; in the last test we have an empty set '(), and we know that automatically we could stop and say
; that the intersectall is empty (saving us some work) ==> we use letcc

(define intersectall-v3
  (lambda (list-of-sets)
    (let/cc hop ; <================================= Added line
      (letrec
        ((∩∩ (lambda (lset)
            (cond
              [(null? (car lset)) (hop '())] ; <==== Added line
              [(null? (cdr lset)) (car lset)]
              [else (intersect-v2 (car lset) (intersectall-v2 (cdr lset)))]))))
        (cond
          [(null? list-of-sets) '()]
          [else (∩∩ list-of-sets)])))))

(module+ test
  [check-equal? (intersectall-v3 '((1 2 3) (2 3 4) (1 4 5 2))) '(2)]
  [check-equal? (intersectall-v3 '()) '()]
  [check-equal? (intersectall-v3 '((3 mangos and) (3 kiwis and) (3 hamburgers))) '(3)]
  [check-equal? (intersectall-v3 '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))) '()]
  [check-equal? (intersectall-v3 '((3 mangoes and) () (3 diet hamburgers))) '()])

; it seems that let/cc defines a function hop, which when called it will exit the function 
; and return its argument (meaning that exits the whole recursion well and returns '()
; FOURTEENTH COMMANDMENT: Use let/cc to return values directly (screw the recursion!)

; we can do better. This is the line where everything happens:
; (intersect (car lset) (intersectall-v2 (cdr lset)))
; but we could also shortcircuit the recursion when the second set is empty => (intersectall-v2 (cdr lset))
; change (intersect to take that into consideration


(define intersect-v3
  (lambda (s1 s2)
    (letrec
      ((∩ (lambda (set)
          (cond
            [(null? set) set]
            [(member? (car set) s2) (cons (car set) (∩ (cdr set)))]
            [else (∩ (cdr set))]))))
      (cond
        [(null? s2) s2]
        [else (∩ s1)]))))

; this helps but doesn't solve the whole problem because in (intersectall...) what we want is to exit directly



