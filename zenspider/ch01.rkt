#lang racket/base

(require "lib/shared.rkt")
(require (only-in racket/function thunk))

;;; Chapter 1

(module+ test
  (require rackunit)

  ;; scheme sanity check:
  (check-false (atom? '()))

  ;; pg 1 - 4
  (check-true (atom? 'atom))
  (check-true (atom? 1492))
  (check-true (list? '(atom)))
  (check-true (list? '(atom turkey or)))

  ;; pg 5 - 7
  (check-true (eq? (car '(a b c)) 'a))
  (check-equal? (car '((a b c) x y z)) '(a b c))
  (check-equal? (car (cdr '((b) (x y) ((c))))) '(x y))
  (check-equal? (cdr (cdr '((b) (x y) ((c))))) '(((c))))

  ;; errors in pedantic scheme
  (check-exn exn:fail? (thunk (car '())))
  (check-exn exn:fail? (thunk (cdr '())))
  (check-exn exn:fail? (thunk (cdr (car '(a (b (c)) d)))))

  ;; pg 8
  (check-equal? (cons '(banana and)
                      '(peanut butter and jelly))
                '((banana and) peanut butter and jelly))
  (check-equal? (cons '((help) this)
                      '(is very ((hard) to learn)))
                '(((help) this) is very ((hard) to learn)))
  (check-equal? (cons '(a b (c))
                      '())
                '((a b (c))))
  (check-equal? (cons 'a '())
                '(a))
  (check-equal? (cons '((a b c))
                      'b)
                '(((a b c)) . b))            ; INTERESTING: not an error in lisp
  (check-equal? (cons 'a 'b)
                '(a . b))                         ; ditto

  ;; pg 9

  (check-equal? '(a b) (cons 'a (car '((b) c d))))
  (check-equal? '(a c d) (cons 'a (cdr '((b) c d))))
  (check-true (null? '()))
  (check-false (null? '(a b c)))
  (check-false (null? 'spaghetti))

  ;; pg 10
  (check-false (atom? '(harry had an apple)))
  (check-false (atom? '()))
  (check-true (atom? 42))
  (check-false (atom? (car (cdr '(swing (low sweet) cherry oat)))))

  ;; pg 11 - 12
  (check-true (eq? 'Harry (quote Harry)))
  (check-false (eq? 'margerine 'butter))
  (check-false (eq? '() '(a)))
  (check-true (eq? 'mary (car '(mary had a little lamb))))
  (check-false (eq? (cdr '(soured milk)) 'milk))
  (check-true (eq? (car (cdr '(soured milk))) 'milk)))
