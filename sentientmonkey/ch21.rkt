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
