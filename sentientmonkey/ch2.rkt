#lang racket/base

(require rackunit)
(require "prelude.rkt")

(provide member?)

; Chapter 2
(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(check-true (lat? '(bacon and eggs)))
(check-false (lat? '(bacon (and eggs))))
(check-false (lat? '((and eggs))))

(check-true (or (null? '()) (atom? '(d e f g))))
(check-true (or (null? '(a b c)) (null? '())))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))

(check-true (member? 'meat '(meat gravy)))
(check-true (member? 'meat '(potatoes and meat gravy)))
(check-true (member? 'meat '(mashed potatoes and meat gravy)))
(check-false (member? 'liver '()))
(check-false (member? 'liver '(lox)))
(check-false (member? 'liver '(bagels and lox)))
