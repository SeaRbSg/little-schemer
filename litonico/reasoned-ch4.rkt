;; I GET IT! 'd' is the 'decrement' part, like in 'cdr',
;; and 'a' is the 'address' part, like in 'car'.
#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define mem
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

[check-equal?
  (mem 'tofu '(a b tofu d peas e))
  '(tofu d peas e)]

(define memo
  (lambda (x l out) ;; NOT an out variable!
    (conde
      [(caro l x) (== out l)]
      [else
        (fresh (tail)
          (cdro l tail)
          (memo x tail out))])))


[check-equal?
  (run 1 (out)
    (memo 'tofu '(a b tofu d peas e) out))
  '((tofu d peas e))]


[check-equal?
  (run 1 (out)
    (fresh (x)
        (memo 'tofu `(a b ,x d tofu e) out)))
  '((tofu d tofu e))]

(run 5 (out)
  (fresh (x)
    (memo 'tofu `(a b ,x d tofu e) out)))
; => '((tofu d tofu e) (tofu e)). Makes sense. Cool.

[check-equal?
  (run* (q)
    (memo 'tofu '(tofu e) '(tofu e))
    (== #t q))
  '(#t)]


[check-equal?
  (run* (out)
    (fresh (x)
      (memo 'tofu `(a b ,x d tofu e) out)))
    '((tofu d tofu e) (tofu e))]

(define rember
  (lambda (x l)
    (cond
      [(null? l) '()]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l)
                  (rember x (cdr l)))])))

[check-equal?
  (rember 'peas '(a b peas d peas e))
  '(a b d peas e)]


(define rembero
  (lambda (x l out)
    (conde
      [(nullo l) (== '() out)]
      [(caro l x) (cdro l out)]
      [else
        (fresh (res a d)
          (cdro l d)
          (rembero x d res)
          (caro l a)
          (conso a res out))])))

[check-equal?
  (run 1 (out)
    (fresh (y)
      (rembero 'peas `(a b ,y d peas e) out)))
  '((a b d peas e))]

(run* (out)
  (fresh (y z)
    (rembero y `(a b ,y d ,z e) out))) ;; This is cool
