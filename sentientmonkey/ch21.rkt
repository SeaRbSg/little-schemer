#lang racket/base

(require rackunit)
(require minikanren)

(define s# (== #f #f))
(define u# (== #f #t))

(check-equal? (run* (q)
                    u#)
              '())

(check-equal? (run* (q)
                    (== #t q))
              '(#t))

(check-equal? (run* (q)
                    u#
                    (== #t q))
              '())

(check-equal? (run* (q)
                    s#
                    (== #t q))
              '(#t))

(check-equal? (run* (q)
                    s#
                    (== 'corn q))
              '(corn))

(check-equal? (run* (q)
                    u#
                    (== 'corn q))
              '())

(check-equal? (run* (q)
                    s#
                    (== #f q))
              '(#f))

(check-equal? (run* (q)
                    (let ((x #t))
                      (== #f x)))
              '())

(check-equal? (run* (q)
                    (let ((x #f))
                      (== #t x)))
              '())

(check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== #t q)))
              '(#t))

(check-equal? (run* (q)
                    (fresh (x)
                           (== x #t)
                           (== #t q)))
              '(#t))

(check-equal? (run* (q)
                    (fresh (x)
                           (== x #t)
                           (== q #t)))
              '(#t))

(check-equal? (run* (x)
                    s#)
              '(_.0))

(check-equal? (run* (x)
                    (let ((x #f))
                      (fresh (x)
                             (== #t x))))
              '(_.0))

(check-equal? (run* (r)
                    (fresh (x y)
                           (== (cons x (cons y '())) r)))
              '((_.0 _.1)))

(check-equal? (run* (s)
                    (fresh (t u)
                           (== (cons t (cons u '())) s)))
              '((_.0 _.1)))

(check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons y (cons x (cons y '()))) r)))))
              '((_.0 _.1 _.0)))

(check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons x (cons y (cons x '()))) r)))))
              '((_.0 _.1 _.0)))

(check-equal? (run* (q)
                    (== #f q)
                    (== #t q))
              '())

(check-equal? (run* (q)
                    (== #f q)
                    (== #f q))
              '(#f))

(check-equal? (run* (q)
                    (let ((x q))
                      (== #t x)))
              '(#t))

(check-equal? (run* (r)
                    (fresh (x)
                           (== x r)))
              '(_.0))

(check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== x q)))
              '(#t))

(check-equal? (run* (q)
                    (fresh (x)
                           (== x q)
                           (== #t x)))
              '(#t))

(check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== x q)))
              '(#t))

(check-equal? (cond
                (#f #f)
                (else u#)) u#)

(check-equal? (cond
                (#f s#)
                (else u#)) u#)

(check-equal? (run* (r)
                    (conde
                      (u# s#))) ; else not needed
              '())

(check-equal? (run* (r)
                    (conde
                      (u# u#)
                      (s#))) ; vs. else
              '(_.0))

(check-equal? (run* (x)
                    (conde
                      ((== 'olive x) s#)
                      ((== 'oil x) s#)))
              '(olive oil))

(check-equal? (run 1 (x)
                   (conde
                     ((== 'olive x) s#)
                     ((== 'oil x) s#)))
              '(olive))

(check-equal? (run* (x)
                    (conde
                      ((== 'virgin x) u#)
                      ((== 'olive x) s#)
                      (s# s#)
                      ((== 'oil x) s#)))
              '(olive _.0 oil))

(check-equal? (run 2 (x)
                   (conde
                     ((== 'extra x) s#)
                     ((== 'virgin x) u#)
                     ((== 'olive x) s#)
                     ((== 'oil x) s#)))
              '(extra olive))

(check-equal? (run* (r)
                    (fresh (x y)
                           (== 'split x)
                           (== 'pea y)
                           (== (cons x (cons y '())) r)))
                    '((split pea)))

(check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                             ((== 'split x) (== 'pea y))
                             ((== 'navy x) (== 'bean y)))
                           (== (cons x (cons y '())) r)))
              '((split pea) (navy bean)))

(check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                             ((== 'split x) (== 'pea y))
                             ((== 'navy x) (== 'bean y)))
                           (== (cons x (cons y (cons 'soup '()))) r)))
              '((split pea soup) (navy bean soup)))

(define (teacupo x)
  (conde
    ((== 'tea x) s#)
    ((== 'cup x) s#)))

(check-equal? (run* (x)
                    (teacupo x))
              '(tea cup))

(check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                             ((teacupo x) (== #t y) s#)
                             ((== #f x) (== #t y)))
                           (== (cons x (cons y '())) r)))
              '((#f #t) (tea #t) (cup #t))) ; need more tea (or rather coffee) to understand...)
