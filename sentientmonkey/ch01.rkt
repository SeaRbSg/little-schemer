#lang racket/base

(require rackunit)
(require "prelude.rkt")

(check-equal? (car '(((hotdogs)) (and) (pickle) relish)) '((hotdogs)))
(check-equal? (car (car '(((hotdogs)) (and) (pickle) relish))) '(hotdogs))
(check-equal? (cdr '(a b c)) '(b c))
(check-equal? (cdr '(hamburger)) '())
(check-equal? (cdr '((x) t r)) '(t r))
(check-equal? (car (cdr '((b) (x y) ((c))))) '(x y))
(check-equal? (cdr (cdr '((b) (x y) ((c))))) '(((c))))

(check-equal? (cons '(banana and) '(peanut butter and jelly)) '((banana and) peanut butter and jelly))
(check-equal? (cons '((help) this) '(is very ((hard) to learn))) '(((help) this) is very ((hard) to learn)))
(check-equal? (cons '(a b (c)) '()) '((a b (c))))
(check-equal? (cons 'a '()) '(a))
(check-equal? (cons 'a (car '((b) c d))) '(a b))
(check-equal? (cons 'a (cdr '((b) c d))) '(a c d))

(check-true (null? (quote ())))
(check-false (null? '(a b c)))

(check-true (atom? 'Harry))
(check-true (atom? (car '(Harry had a heap of apples))))
(check-false (atom? (cdr '(Harry had a heap of apples))))
(check-false (atom? (cdr '(Harry))))
(check-true (atom? (car (cdr '(swing low sweet cherry oat)))))
(check-false (atom? (car (cdr '(swing (low sweet) cherry oat)))))

(check-true (eq? 'Harry 'Harry))
(check-false (eq? 'margarine 'butter))
(check-false (eq? '() '(strawberry)))
(check-false (eq? '6 '7))
(check-true (eq? (car '(Mary had a little lamb chop)) 'Mary))
(check-false (eq? (cdr '(soured milk)) 'milk))

(check-false (eq? (cdr '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans)))))

