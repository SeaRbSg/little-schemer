#lang racket/base

(require rackunit)
(require minikanren)
(require "reasoned.rkt")

;; 1
(define c 42)

(check-equal?
  (let ([x (lambda (a) a)]
        [y c])
    (x y))
  c)

;; 2
(check-run* (r)
            (fresh (y x)
                   (== (list x y) r))
            => '((_.0 _.1)))

;; 3
(check-run* (r)
            (fresh (v w)
                   (== (let ([x v]
                             [y w])
                         (list x y)) r))
            => '((_.0 _.1)))

;; 4
(check-equal? (car '(grape rasin pear)) 'grape)

;; 5
(check-equal? (car '(a c o r n)) 'a)

;; 9
(define (caro p a)
  (fresh (d)
         (== (cons a d) p)))

;; 6
(check-run* (r)
            (caro '(a c o r n) r)
            => '(a))

;; 7
(check-run* (q)
            (caro '(a c o r n) 'a)
            (== #t q)
            => '(#t))

;; 8
(check-run* (r)
            (fresh (x y)
                   (caro (list r y) x)
                   (== 'pear x))
            => '(pear))

;; 10
(check-equal? (cons
                (car '(grape raisin pear))
                (car '((a) (b) (c))))
              '(grape a))

;; 11
(check-run* (r)
            (fresh (x y)
                   (caro '(grape raisin pear) x)
                   (caro '((a) (b) (c)) y)
                   (== (cons x y) r))
            => '((grape a)))

;; 13
(check-equal? (cdr '(grape raisin pear)) '(raisin pear))

;; 14
(check-equal? (car (cdr '(a c o r n))) 'c)

;; 16
(define (cdro p d)
  (fresh (a)
         (== (cons a d) p)))

;; 15
(check-run* (r)
            (fresh (v)
                   (cdro '(a c o r n) v)
                   (caro v r))
            => '(c))

;; 17
(check-equal? (cons
                (cdr '(grape raisin pear))
                (car '((a) (b) (c))))
              '((raisin pear) a))

;; 18
(check-run* (r)
            (fresh (x y)
                   (cdro '(grape raisin pear) x)
                   (caro '((a) (b) (c)) y)
                   (== (cons x y) r))
            => '(((raisin pear) a)))

