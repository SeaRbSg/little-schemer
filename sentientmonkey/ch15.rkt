#lang racket/base

(require rackunit)
(require "prelude.rkt")

(define x
  (cons 'chicago
    (cons 'pizza
      '())))

(check-equal? x '(chicago pizza))

(set! x 'gone)

(check-equal? x 'gone)

(set! x 'skins)

(check-equal? x 'skins)

(define (gourmet food)
  (cons food
    (cons x '())))

(check-equal? (gourmet 'onion) '(onion skins))

(set! x 'rings)

(check-equal? (gourmet 'onion) '(onion rings))

(define (gourmand food)
  (set! x food)
  (cons food
    (cons x '())))

(test-case "gourmand"
  (check-equal? (gourmand 'potato) '(potato potato))
  (check-equal? x 'potato)
  (check-equal? (gourmand 'rice) '(rice rice))
  (check-equal? x 'rice))

(define (diner food)
  (cons 'milkshake
    (cons food '())))

(check-equal? (diner 'onion) '(milkshake onion))

(define (dinerR food)
  (set! x food)
  (cons 'milkshake
    (cons food '())))

(test-case "dinerR"
  (check-equal? (dinerR 'onion) '(milkshake onion))
  (check-equal? x 'onion)
  (check-equal? (dinerR 'pecanpie) '(milkshake pecanpie))
  (check-equal? x 'pecanpie)
  (check-equal? (gourmand 'onion) '(onion onion)))

(define omnivore
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

(test-case "omnivore"
  (check-equal? (omnivore 'bouillabaisse) '(bouillabaisse bouillabaisse))
  (check-equal? x 'onion))

(define gobbler
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

(test-case "gobbler"
  (check-equal? (gobbler 'gumbo) '(gumbo gumbo))
  (check-equal? x 'onion))

(define food '()) ; had to define food before set!ing the value

(define (glutton x)
  (set! food x)
  (cons 'more
    (cons x
      (cons 'more
        (cons x '())))))

(test-case "glutton"
  (check-equal? (glutton 'onion) '(more onion more onion))
  (check-equal? food 'onion)
  (check-equal? (glutton 'garlic) '(more garlic more garlic))
  (check-equal? food 'garlic)
  (check-equal? x 'onion))

(define (chez-nous)
  (let ([old-food food])
    (set! food x)
    (set! x old-food)))

(test-case "chez-nous"
  (chez-nous)
  (check-equal? food 'onion)
  (check-equal? x 'garlic))

(test-case "more chez-nous"
  (check-equal? (glutton 'garlic) '(more garlic more garlic))
  (check-equal? (gourmand 'potato) '(potato potato))
  (check-equal? x 'potato)
  (chez-nous)
  (check-equal? food 'potato)
  (check-equal? x 'garlic))


