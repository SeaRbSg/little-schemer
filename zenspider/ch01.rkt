#lang racket/base

(require "lib/shared.rkt")
(require rackunit)
(require (only-in racket/function thunk))

;; scheme sanity check:
(test #f (atom? '()))

;;; Chapter 1
;; pg 1 - 4
(test #t (atom? 'atom))
(test #t (atom? 1492))
(test #t (list? '(atom)))
(test #t (list? '(atom turkey or)))

;; pg 5 - 7
(test #t (eq? (car '(a b c)) 'a))
(test (car '((a b c) x y z)) '(a b c))
(test (car (cdr '((b) (x y) ((c))))) '(x y))
(test (cdr (cdr '((b) (x y) ((c))))) '(((c))))

;; errors in pedantic scheme
(check-exn exn:fail? (thunk (car '())))
(check-exn exn:fail? (thunk (cdr '())))
(check-exn exn:fail? (thunk (cdr (car '(a (b (c)) d)))))

;; pg 8
(test (cons '(banana and)
            '(peanut butter and jelly))
      '((banana and) peanut butter and jelly))
(test (cons '((help) this)
            '(is very ((hard) to learn)))
      '(((help) this) is very ((hard) to learn)))
(test (cons '(a b (c))
            '())
      '((a b (c))))
(test (cons 'a '())
      '(a))
(test (cons '((a b c))
            'b)
      '(((a b c)) . b))            ; INTERESTING: not an error in lisp
(test (cons 'a 'b)
      '(a . b))                         ; ditto

;; pg 9

(test '(a b) (cons 'a (car '((b) c d))))
(test '(a c d) (cons 'a (cdr '((b) c d))))
(test #t (null? '()))
(test #f (null? '(a b c)))
(test #f (null? 'spaghetti))

;; pg 10
(test #f (atom? '(harry had an apple)))
(test #f (atom? '()))
(test #t (atom? 42))
(test #f (atom? (car (cdr '(swing (low sweet) cherry oat)))))

;; pg 11 - 12
(test #t (eq? 'Harry (quote Harry)))
(test #f (eq? 'margerine 'butter))
(test #f (eq? '() '(a)))
(test #t (eq? 'mary (car '(mary had a little lamb))))
(test #f (eq? (cdr '(soured milk)) 'milk))
(test #t (eq? (car (cdr '(soured milk))) 'milk))
