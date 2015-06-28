#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch27.rkt")

;; 10
(define *o
  (lambda (n m p)
    (condi
     [(== '() n) (== '() p)]
     [(poso n) (== '() m) (== '() p)]
     [(== '(1) n) (poso m) (= m p)]
     [(>1o n) (== '(1) m) (== n p)]
     [(fresh (x z)
             (== `(0 . ,x) n) (poso x)
             (== `(0 . ,z) p) (poso z)
             (>1o m)
             (*o x m z))]
     [(fresh (x y)
             (== `(1 . ,x) n) (poso x)
             (== `(0 . ,y) m) (poso y)
             (*o m n p))]
     [(fresh (x y)
             (== `(1 . ,x) n) (poso x)
             (== `(1 . ,y) m) (poso y)
             (odd-*o x n m p))])))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
           (bound-*o q p n m)
           (*o x m q)
           (+o `(0 . ,q) m p))))

(define bound-*o
  (lambda (q p n m)
    s#))
