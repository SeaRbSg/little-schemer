#lang racket/base

(require rackunit)
(require "../lib/j-bob-lang.rkt")
(require "../lib/j-bob.rkt")

(check-equal?
  (J-Bob/step (prelude)
            '(car (cons 'ham '(cheese)))
            '())
  '(car (cons 'ham '(cheese))))


(check-equal?
  (J-Bob/step (prelude)
            '(car (cons 'ham '(cheese)))
            '((() (car/cons 'ham '(cheese)))))
  ''ham)

(check-equal?
  (J-Bob/step (prelude)
            '(equal 'flapjack (atom (cons a b)))
            '(((2) (atom/cons a b))
              (() (equal 'flapjack 'nil))))
  ''nil)

