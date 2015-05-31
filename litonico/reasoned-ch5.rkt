#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))

[check-equal?
  (append '(a b c) '(d e)) '(a b c d e)]

(define appendo ;; still confused on this one...
  (lambda (l s out)
    (conde
      [(nullo l) (== s out)]
      [else
        (fresh (head tail res)
          (caro l head)
          (cdro l tail)
          (appendo tail s res)
          (conso head res out))])))

[check-equal?
  (run* (x)
    (appendo '(cake) '(tastes yummy) x))
  '((cake tastes yummy))]


[check-equal?
  (run* (x)
    (fresh (y)
      (appendo `(cake with ice ,y) '(tastes yummy) x)))
  '((cake with ice _.0 tastes yummy))]

[check-equal?
  (run* (x)
    (fresh (y)
      (appendo `(cake with ice cream) y x)))
  '((cake with ice cream . _.0))]

;; What the check is '(d t) supposed to mean)
[check-equal?
  (run 1 (x)
    (fresh (y)
      (appendo `(cake with ice . ,y) '(d t) x)))
  '((cake with ice d t))]

(set! appendo ;; this makes more sense to me
  (lambda (l s out)
    (conde
      [(nullo l) (== s out)]
      [else
        (fresh (head tail res)
          (conso head tail l)
          (appendo tail s res)
          (conso head res out))])))

(run 5 (x) ;; I do not like typing the answers out
  (fresh (y)
    (appendo `(cake with ice . ,y) '(d t) x)))

(run 5 (y)
  (fresh (x)
    (appendo `(cake with ice . ,y) '(d t) x))) ;; -> (== y anylist)

[check-equal?
  (run* (x)
    (fresh (z)
        (appendo `(cake with ice cream) `(d t . ,z) x)))
  '((cake with ice cream d t . _.0))]

(run 6 (x) ;; Why does (run 7 ...) fail to converge?
  (fresh (y)
    (appendo x y '(cake with ice d t))))

(run 6 (y)
  (fresh (x)
    (appendo x y '(cake with ice d t))))

(run 6 (r)
  (fresh (x y)
    (appendo x y '(cake with ice d t))
    (== `(,x ,y) r)))

; (run 7 (r) ;; Ok-- there's no 7th value, so it will try EVERYTHING
;  (fresh (x y)
;    (appendo x y '(cake with ice d t))
;    (== `(,x ,y) r)))

(set! appendo
  (lambda (l s out)
    (conde
      [(nullo l) (== s out)]
      [else
        (fresh (head tail res)
          (conso head tail l)
          (conso head res out)
          (appendo tail s res))])))

(run 7 (x) ;; Don't understand why that change does anything
  (fresh (y z)
    (appendo x y z)))

(run 7 (y)
  (fresh (x z)
    (appendo x y z)))

(run 7 (r)
  (fresh (x y z)
    (appendo x y z)
    (== `(,x ,y ,z) r)))

(define swappendo
  (lambda (l s out)
    (conde
      [%s
        (fresh (head tail res)
          (conso head tail l)
          (conso head res out)
          (appendo tail s res))]
      [else (nullo l) (== s out)]))) ;; This last line doesn't make sense to me
                                     ;; why not just (== s out)
                                     ;; what does (else (nullo l) ...) do?
                                     ;; Also, it's never reached.

; (run 1 (z)
;   (fresh (x y)
;     (swappendo x y z)))

(define unwrap ;; recursively grabs the car
  (lambda (x)
    (cond
      [(pair? x) (unwrap (car x))]
      [else x])))

[check-equal?
  (unwrap '((((pizza)))))
  'pizza]


[check-equal?
  (unwrap '((((pizza pie) with))extra cheese))
  'pizza]

(define unwrapo ;; I still fundamentally do not understand %u and %s and
  (lambda (x out); how they relate to unification.
    (conde
      [(pairo x)
       (fresh (head)
         (caro x head)
         (unwrapo head out))]
      [else (== x out)])))

(run* (x)
  (unwrapo '(((pizza))) x)) ;; Welp, don't get this at all
                            ;; how would, say, '((pizza)) EVER be
                            ;; unified with `out`?

; (run 1 (x)
;   (unwrapo `((,x)) 'pizza))
;
(set! unwrapo
  (lambda (x out)
    (conde
      [%s (== x out)] ;; BUT NOW WHY DO WE NEED A CONDE AT ALL?
      [else
       (fresh (head)
         (caro x head)
         (unwrapo head out))])))

(run 5 (x)
  (unwrapo x '((pizza)))) ;; With the recursion switched, this makes sense

(define flatten
  (lambda (s)
    (cond
      [(null? s) '()]
      [(pair? s)
       (append
         (flatten (car s))
         (flatten (cdr s)))]
      [else (cons s '())])))

[check-equal? (flatten '((a b) c)) '(a b c)]

(define flatteno
  (lambda (s out)
    (conde
      [(nullo s) (== '() out)]
      [(pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out))]
      [else (conso s '() out)])))


[check-equal?
  (run 1 (x)
    (flatteno '((a b) c) x))
  '((a b c))]

(run* (x)
  (flatteno '(a) x)) ;; Wat

(define flattenrevo
  (lambda (s out)
    (conde
      [%s (conso s '() out)]
      [(nullo s) (== '() out)]
      [else
       (fresh (a d res-a res-d)
         (conso a d s)
         (flattenrevo a res-a)
         (flattenrevo d res-d)
         (appendo res-a res-d out))])))

(run 2 (x) ;; Halp
  (flattenrevo x '(a b c)))
