#lang racket

(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food
                (quote())))))

(displayln (dinerR '(quote onion)))