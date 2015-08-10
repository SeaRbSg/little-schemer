#lang racket/base

(require rackunit)
(require "../lib/j-bob-lang.rkt")
(require "../lib/j-bob.rkt")

(check-equal? (car (cons 'ham '(eggs))) 'ham)
(check-equal? (atom '()) 't)
(check-equal? (atom (cons 'ham '(eggs))) 'nil)
(check-equal? (equal 'flapjack 'nil) 'nil)

(dethm atom/cons (x y)
       (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
       (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
       (equal (cdr (cons x y)) y))

(check-equal? (equal 'eggs '(ham)) 'nil)

(check-equal? (car (cons 't '(and crumpets))) 't)

(dethm equal-same (x)
       (equal (equal x x) 't))

(dethm equal-swap (x y)
       (equal (equal x y) (equal y x)))
