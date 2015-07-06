#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch22.rkt")
(require "ch27.rkt")

(define (bound-*o q p n m)
  (conde
    [(nullo q) (pairo p)]
    [else
      (fresh (x y z)
        (cdro q x)
        (cdro p y)
        (condi
          [(nullo n)
           (cdro m z)
           (bound-*o x y z '())]
          [else
            (cdro n z)
            (bound-*o x y z m)]))]))

(define (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (+o `(0 . ,q) m p)))

(define (*o n m p)
  (condi
    [(== '() n) (== '() p)]
    [(poso n) (== '() m) (== '() p)]
    [(== '(1) n) (poso m) (== m p)]
    [(>1o m) (== '(1) m) (== n p)]
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
       (odd-*o x n m p))]
    [else u#]))

(check-run* (t)
            (fresh (n m)
              (*o n m '(1))
              (== `(,n ,m) t))
            => '(((1) (1))))

(check-run* (p)
            (*o '(1 1 1) '(1 1 1 1 1 1) p)
            => '((1 0 0 1 1 1 0 1 1)))

(define (=lo n m)
  (conde
    [(== '() n) (== '() m)]
    [(== '(1) n) (== '(1) m)]
    [else
      (fresh (a x b y)
        (== `(,a . ,x) n) (poso x)
        (== `(,b . ,y) m) (poso y)
        (=lo x y))]))

(check-run* (t)
            (fresh (w x y)
              (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))
              (== `(,w ,x ,y) t))
            => '((_.0 _.1 (_.2 1))))

(check-run* (b)
           (=lo '(1) `(,b))
           => '(1))

(check-run* (n)
            (=lo `(1 0 1 . ,n) '(0 1 1 0 1))
            => '((_.0 1)))

(define (<lo n m)
  (conde
    [(== '() n) (poso m)]
    [(== '(1) n) (>1o m)]
    [else
      (fresh (a x b y)
        (== `(,a . ,x) n) (poso x)
        (== `(,b . ,y) m) (poso y)
        (<lo x y))]))

(check-run 2 (t)
           (fresh (y z)
             (<lo `(1 . ,y) '(0 1 1 0 1 . ,z))
             (== `(,y ,z) t))
           => '((() _.0)
                ((1) _.0)))

(define (<o n m)
  (condi
    [(<lo n m) s#]
    [(=lo n m)
     (fresh (x)
       (poso x)
       (+o n x m))]
    [else u#]))

(define (<=o n m)
  (condi
    [(== n m) s#]
    [(<o n m) s#]
    [else u#]))

(check-run* (q)
            (<o '(1 0 1) '(1 1 1))
            (== #t q)
            => '(#t))

(check-run* (q)
            (<o '(1 1 1) '(1 0 1))
            (== #t q)
            => '())

(check-run* (q)
            (<o '(1 0 1) '(1 0 1))
            (== #t q)
            => '())

(check-run* (n)
            (<o n '(1 0 1))
            => '(()
                 (0 0 1)
                 (1)
                 (_.0 1)))

(check-run* (m)
            (<o '(1 0 1) m)
            => '((_.0 _.1 _.2 _.3 . _.4)
                 (0 1 1)
                 (1 1 1)))

(define (÷o n m q r)
  (condi
    [(== '() q) (== n r) (<o n m)]
    [(== '(1) q) (== '() r) (== n m)
     (<o r m)]
    [(<o m n) (<o r m)
     (fresh (mq)
       (<=o mq n)
       (*o m q mq)
       (+o mq r n))]
    [else u#]))

(check-run* (m)
            (fresh (r)
              (÷o '(1 0 1) m '(1 1 1) r))
            => '())
