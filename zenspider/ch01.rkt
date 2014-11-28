#lang racket/base

(require "lib/shared.rkt")
(require rackunit)
(require (only-in racket/function thunk))

;; scheme sanity check:
(check-equal? #f (atom? '()))

;;; Chapter 1
;; pg 1 - 4
(check-equal? #t (atom? 'atom))
(check-equal? #t (atom? 1492))
(check-equal? #t (list? '(atom)))
(check-equal? #t (list? '(atom turkey or)))

;; pg 5 - 7
(check-equal? #t (eq? (car '(a b c)) 'a))
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
(check-equal? #t (null? '()))
(check-equal? #f (null? '(a b c)))
(check-equal? #f (null? 'spaghetti))

;; pg 10
(check-equal? #f (atom? '(harry had an apple)))
(check-equal? #f (atom? '()))
(check-equal? #t (atom? 42))
(check-equal? #f (atom? (car (cdr '(swing (low sweet) cherry oat)))))

;; pg 11 - 12
(check-equal? #t (eq? 'Harry (quote Harry)))
(check-equal? #f (eq? 'margerine 'butter))
(check-equal? #f (eq? '() '(a)))
(check-equal? #t (eq? 'mary (car '(mary had a little lamb))))
(check-equal? #f (eq? (cdr '(soured milk)) 'milk))
(check-equal? #t (eq? (car (cdr '(soured milk))) 'milk))
