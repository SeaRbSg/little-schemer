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
  [check-equal? (intersectall-v2 '((3 mangoes and) () (3 diet hamburgers))) '()])

; in the last test we have an empty set '(), and we know that automatically we could stop and say
; that the intersectall is empty (saving us some work) ==> we use letcc

(define intersectall-v3
  (lambda (list-of-sets)
    (let/cc hop ; <================================= Added line :: Define scope and the exit of the worm-hole
      (letrec
        ((∩∩ (lambda (lset)
            (cond
              [(null? (car lset)) (hop '())] ; <==== Added line :: Entrance to the worm-hole and passenger sent
              [(null? (cdr lset)) (car lset)]
              [else (intersect-v2 (car lset) (intersectall-v3 (cdr lset)))]))))
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

; The way I visualize it. All lets are frames / boxes / scopes / space-time continuum frames of reference
; Inside the (let/cc we define a scope with two things:
;   - 1st: definition ==> the exit of a worm-hole        :: the same as with letrecc
;   - 2nd: value      ==> the code as it is evaluated    :: the same as with letrecc
; It works the same as letrecc, but now instead of having the definition available through the recursion,
; the definition is an exit point
; Inside the value part, we can enter the worm-hole by passing something to get through the worm-hole (a return value)
; but I disgress, getting back to the code...
;
; ...we can do better. This is the line where everything happens:
; (intersect (car lset) (intersectall-v2 (cdr lset)))
; but we could also shortcircuit the recursion when the second set is empty => (intersectall-v2 (cdr lset))
; change (intersect to take that into consideration

; this helps but doesn't solve the whole problem because in (intersectall...) what we want is to exit directly
; do two things:
; 1.- letreccify intersect inside intersectall
; 2.- when we encounter s2 empty just hop

(define intersectall-v4
  (lambda (list-of-sets)
    (let/cc hop
      (letrec
        ((intersect (lambda (s1 s2)
             (letrec
               ((∩ (lambda (s0)
                   (cond
                     [(null? s0) s0]
                     [(member? (car s0) s2) (cons (car s0) (∩ (cdr s0)))]
                     [else (∩ (cdr s0))]))))
               (cond
                 ;[(null? s2) s2]       ; Since we are in the same space-time ref,
                 [(null? s2) (hop '())] ; we can go through the worm-hole!
                 [else (∩ s1)]))))
         (∩∩ (lambda (lset)
            (cond
              ;[(null? (car lset)) (hop '())] ; no need now, we are already dealing with it inside
              [(null? (cdr lset)) (car lset)]
              [else (intersect (car lset) (intersectall-v4 (cdr lset)))]))))
        (cond
          [(null? list-of-sets) '()]
          [else (∩∩ list-of-sets)])))))

(module+ test
  [check-equal? (intersectall-v4 '((1 2 3) (2 3 4) (1 4 5 2))) '(2)]
  [check-equal? (intersectall-v4 '()) '()]
  [check-equal? (intersectall-v4 '((3 mangos and) (3 kiwis and) (3 hamburgers))) '(3)]
  [check-equal? (intersectall-v4 '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))) '()]
  [check-equal? (intersectall-v4 '((3 mangoes and) () (3 diet hamburgers))) '()])

; because I am a gluton for pain, lets refactor it a bit and see if it works
; STEP 1

(define intersectall-v5
  (lambda (list-of-sets)
    (let/cc hop
      (letrec
        ((∩∩ (lambda (lset)
          (letrec
              ((intersect (lambda (s1 s2)
                (letrec
                  ((∩ (lambda (s0)
                   (cond
                     [(null? s0) s0]
                     [(member? (car s0) s2) (cons (car s0) (∩ (cdr s0)))]
                     [else (∩ (cdr s0))]))))
                  (cond
                    [(null? s2) (hop '())]
                    [else (∩ s1)])))))
              (cond
                [(null? lset) (hop '())] ; <-- add this to get rid of top level letrec value!
                [(null? (cdr lset)) (car lset)]
                [else (intersect (car lset) (intersectall-v5 (cdr lset)))])))))
        ;(cond
        ;  [(null? list-of-sets) '()]
        ;  [else (∩∩ list-of-sets)])
        (∩∩ list-of-sets)))))

; STEP 2
; now we can get rid of top level letrec !!

(define intersectall-v6
  (lambda (list-of-sets)
    (let/cc hop
      ;(letrec
      ;  ((∩∩ (lambda (lset)
          (letrec
              ((intersect (lambda (s1 s2)
                (letrec
                  ((∩ (lambda (s0)
                   (cond
                     [(null? s0) s0]
                     [(member? (car s0) s2) (cons (car s0) (∩ (cdr s0)))]
                     [else (∩ (cdr s0))]))))
                  (cond
                    [(null? s2) (hop '())]
                    [else (∩ s1)])))))
              (cond
                [(null? list-of-sets) '()]
                [(null? (cdr list-of-sets)) (car list-of-sets)]
                [else (intersect (car list-of-sets) (intersectall-v6 (cdr list-of-sets)))])))))
        ;(∩∩ list-of-sets))))

; FINAL FORM REFACTORED :: beautiful!

(define intersectall-v7
  (lambda (lset)
    (let/cc hop
      (letrec
        ((intersect (lambda (s1 s2)
            (letrec
              ((∩ (lambda (s0)
                  (cond
                    [(null? s0) s0]
                    [(member? (car s0) s2) (cons (car s0) (∩ (cdr s0)))]
                    [else (∩ (cdr s0))]))))
              (cond
                [(null? s2) (hop '())]
                [else (∩ s1)])))))
        (cond
          [(null? lset) '()]
          [(null? (cdr lset)) (car lset)]
          [else (intersect (car lset) (intersectall-v7 (cdr lset)))])))))

(module+ test
  [check-equal? (intersectall-v7 '((1 2 3) (2 3 4) (1 4 5 2))) '(2)]
  [check-equal? (intersectall-v7 '()) '()]
  [check-equal? (intersectall-v7 '((3 mangos and) (3 kiwis and) (3 hamburgers))) '(3)]
  [check-equal? (intersectall-v7 '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))) '()]
  [check-equal? (intersectall-v7 '((3 mangoes and) () (3 diet hamburgers))) '()])

;;;;;; BREATHE ;;;;;;;

; write rember letreccified

(define rember-original
  (lambda (a lat)
    (cond
      [(null? lat) lat]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember-original a (cdr lat)))])))

(define rember
  (lambda (a lat)
    (letrec
        ((✄ (lambda (l)
            (cond
              [(null? l) l]
              [(eq? a (car l)) (cdr l)]
              [else (cons (car l) (✄ (cdr l)))]))))
        (✄ lat))))

(module+ test
  [check-equal? (rember '2 '(1 2 3)) '(1 3)])
  
; write rember-beyond-first (which is a shitty name so I will call it slice-upto

(define slice-upto
  (lambda (a lat)
    (letrec
        ((✄ (lambda (l)
            (cond
              [(null? l) '()]
              [(eq? a (car l)) '()]
              [else (cons (car l) (✄ (cdr l)))]))))
        (✄ lat))))

(module+ test
  [check-equal? (slice-upto 'roots '(noodles wtf bla ouch roots potatoes yam others rice)) 
                                   '(noodles wtf bla ouch)]
  [check-equal? (slice-upto 'rice  '(noodles wtf bla ouch roots potatoes yam others rice)) 
                                   '(noodles wtf bla ouch roots potatoes yam others)]
  [check-equal? (slice-upto 'pepes '(noodles wtf bla ouch roots potatoes yam others rice)) 
                                   '(noodles wtf bla ouch roots potatoes yam others rice)])

; now write the complementary slice-beyond
; I want to go over each atom of the list, and when we find it, we restart!
; if not, we return the whole list

(define slice-beyond
  (lambda (a lat)
    (let/cc reset
      (letrec
          ((✄ (lambda (l)
              (cond
                [(null? l) l]
                [(eq? a (car l)) (reset (✄ (cdr l)))]
                [else (cons (car l) (✄ (cdr l)))]))))
          (✄ lat)))))

(module+ test
  [check-equal? (slice-beyond 'roots '(noodles wtf bla ouch roots potatoes yam others rice)) 
                                     '(potatoes yam others rice)]
  [check-equal? (slice-beyond 'rice  '(noodles wtf bla ouch roots potatoes yam others rice)) 
                                     '()]
  [check-equal? (slice-beyond 'pepes '(noodles wtf bla ouch roots potatoes yam others rice))
                                     '(noodles wtf bla ouch roots potatoes yam others rice)])
