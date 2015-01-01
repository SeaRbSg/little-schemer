#lang racket/base

(require rackunit)
(require "preface.rkt")

;; Atoms

(check-true (atom? 'atom))
(check-true (atom? 'turkey))
(check-true (atom? '1492))
(check-true (atom? 'u))
(check-true (atom? '*abc&))

;; Car

(check-equal? (car '(a b c)) 'a)
(check-equal? (car '((a b c) x y z)) '(a b c))
(check-equal? (car '(((hotdogs)) (and) (pickle) relish)) '((hotdogs)))
(check-equal? (car (car '(((hotdogs)) (and) (pickle) relish))) '(hotdogs))

;; Cdr

(check-equal? (cdr '(a b c)) '(b c))
(check-equal? (cdr '((a b c) x y z)) '(x y z))
(check-equal? (cdr '(hamburger)) '())
(check-equal? (cdr '((x) t r)) '(t r))

(check-equal? (car (cdr '((b) (x y) ((c))))) '(x y))
(check-equal? (cdr (cdr '((b) (x y) ((c))))) '(((c))))
(check-equal? (cdr (car '((b) (x y) ((c))))) '())

;; Cons

(check-equal? (cons '(banana and) '(peanut butter and jelly)) '((banana and) peanut butter and jelly))
(check-equal? (cons '((help) this) '(is very ((hard) to learn))) '(((help) this) is very ((hard) to learn)))
(check-equal? (cons 'a '()) '(a))

;; Null

(check-true (null? '()))
(check-false (null? '(a b c)))

(check-false (atom? '(Harry had a heap of apples)))
(check-true (atom? (car '(Harry had a heap of apples))))

;; Eq

(check-false (eq? 'margerine 'butter))
(check-true (eq? 'Harry 'Harry))
