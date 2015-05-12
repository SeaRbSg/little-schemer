#lang racket
(require "../lib/mk.rkt")
(require rackunit)

(check-equal? ;; '==' associates items that are matched up in two lists
 (run* (r)
       (fresh (x y z)
              (== (cons x (cons y (cons z '())))
                  '(q r s))
              (== (cons x (cons y (cons  z '()))) r)))
 '((q r s)))

(check-equal? ;; '==' when given two pairs associates the repective values of the pair
 (run* (r)
       (fresh (x y)
              (== (cons x y)
                  (cons 'q 'r))
              (== (cons x (cons y '())) r)))
 '((q r)))

(check-equal? ;; '==' when given a pair and a list assocates the first item in the list with the first
 (run* (r)    ;; value in the pair and the cdr with the second item. Makes sense since a list is a pair.
       (fresh (x y)
              (== (cons x y)
                  '(q r s))
              (== (cons x (cons y '())) r)))
 '((q (r s))))

;; These functions are basically just different ways of applying '==' to pairs
(define caro
  (lambda (p a)
    (fresh (d)
           (conso a d p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (conso a d p))))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(check-equal?
 (run* (r)
       (caro '(hello world) r))
 '(hello))

(check-equal?
 (run* (r)
       (cdro '(a c o r n) '(c o r n))
       (== #t r))
 '(#t))

(check-equal?
 (run* (x)
       (cdro '(c o r n)
             (cons x (cons 'r (cons 'n '())))))
 '(o))

(check-equal? 
 (run* (r)
       (fresh (x y)
              (cdro '(grape raisin pear) x)
              (caro '((a) (b) (c)) y)
              (== (cons x y) r)))
 '(((raisin pear) a)))

(check-equal?
 (run* (r)
       (conso '(a b c) '(d e) r))
 '(((a b c) d e)))

(check-equal?
 (run* (r)
       (conso r '(a b c) '(d a b c)))
 '(d))

(check-equal?
 (run* (q)
       (nullo '(grape raisin pear)))
 '())

(check-equal?
 (run* (q)
       (nullo '())
       (== #t q))
 '(#t))

(check-equal?
 (run* (q)
       (nullo q))
 '(()))

(check-equal?
 (run* (r)
       (pairo (cons r r))
       (== #t r))
 '(#t))

(check-equal?
 (run* (r)
       (eqo 'a 'a)
       (== #t r))
 '(#t))

(check-equal?
 (run* (r)
       (eqo r 'a))
 '(a))
