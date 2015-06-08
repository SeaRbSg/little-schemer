#lang racket

(require "lib/shared.rkt")
(require rackunit)

(define lat?                               ; define names a function
  (lambda (l)                              ; lambda creates a function with arguments
    (cond                                  ; cond asks questions
      ((null? l) #t)                       ; see 1st commandment
      ((atom? (car l)) (lat? (cdr l)))     ; a list with 2 S-expressions !! a question + another question
    (else #f))))

(module+ test
  (check-true  (lat?'(Jack Sprat could eat no chicken fat)))
  (check-true  (lat?'(bacon and eggs)))
  (check-false (lat?'(bacon (and eggs)))))

(module+ test
  (check-true  (or (null? '()) (atom? '(d e f g))))
  (check-false (or (null? '(a b c)) (atom? '(atom))))) ; '(a b c) is not null

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

; FIRST COMMANDMENT: Always, always, always ask null? first in a function

(module+ test
  (check-true  (member? 'meat '(mashed potatoes and meat gravy)))
  (check-false (member? 'liver '(bagels and lox))))
