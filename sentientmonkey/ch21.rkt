#lang racket/base

(require rackunit)
(require minikanren)
(require "reasoned.rkt")

(check-run* (q)
            u#
            => '())

(check-run* (q)
            (== #t q)
            => '(#t))

(check-run* (q)
            u#
            (== #t q)
            => '())

(check-run* (q)
            s#
            (== #t q)
            => '(#t))

(check-run* (q)
            s#
            (== 'corn q)
            => '(corn))

(check-run* (q)
            u#
            (== 'corn q)
            => '())

(check-run* (q)
            s#
            (== #f q)
            => '(#f))

(check-run* (q)
            (let ([x #t])
              (== #f x))
            => '())

(check-run* (q)
            (let ([x #f])
              (== #t x))
            => '())

(check-run* (q)
            (fresh (x)
                   (== #t x)
                   (== #t q))
            => '(#t))

(check-run* (q)
            (fresh (x)
                   (== x #t)
                   (== #t q))
            => '(#t))

(check-run* (q)
            (fresh (x)
                   (== x #t)
                   (== q #t))
            => '(#t))

(check-run* (x)
            s#
            => '(_.0))

(check-run* (x)
            (let ([x #f])
              (fresh (x)
                     (== #t x)))
            => '(_.0))

(check-run* (r)
            (fresh (x y)
                   (== (cons x (cons y '())) r))
            => '((_.0 _.1)))

(check-run* (s)
            (fresh (t u)
                   (== (cons t (cons u '())) s))
            => '((_.0 _.1)))

(check-run* (r)
            (fresh (x)
                   (let ([y x])
                     (fresh (x)
                            (== (cons y (cons x (cons y '()))) r))))
            => '((_.0 _.1 _.0)))

(check-run* (r)
            (fresh (x)
                   (let ([y x])
                     (fresh (x)
                            (== (cons x (cons y (cons x '()))) r))))
            => '((_.0 _.1 _.0)))

(check-run* (q)
            (== #f q)
            (== #t q)
            => '())

(check-run* (q)
            (== #f q)
            (== #f q)
            => '(#f))

(check-run* (q)
            (let ([x q])
              (== #t x))
            => '(#t))

(check-run* (r)
            (fresh (x)
                   (== x r))
            => '(_.0))

(check-run* (q)
            (fresh (x)
                   (== #t x)
                   (== x q))
            => '(#t))

(check-run* (q)
            (fresh (x)
                   (== x q)
                   (== #t x))
            => '(#t))

(check-run* (q)
            (fresh (x)
                   (== #t x)
                   (== x q))
            => '(#t))

(check-equal? (cond
                (#f #f)
                (else u#)) u#)

(check-equal? (cond
                (#f s#)
                (else u#)) u#)

(check-run* (r)
            (conde
              (u# s#)
              (else u#))
            => '())

(check-run* (r)
            (conde
              (u# u#)
              (else s#))
            => '(_.0))

(check-run* (x)
            (conde
              ((== 'olive x) s#)
              ((== 'oil x) s#)
              (else u#))
            => '(olive oil))

(check-run 1 (x)
           (conde
             ((== 'olive x) s#)
             ((== 'oil x) s#)
             (else u#))
           => '(olive))

(check-run* (x)
            (conde
              ((== 'virgin x) u#)
              ((== 'olive x) s#)
              (s# s#)
              ((== 'oil x) s#)
              (else u#))
            => '(olive _.0 oil))

(check-run 2 (x)
           (conde
             ((== 'extra x) s#)
             ((== 'virgin x) u#)
             ((== 'olive x) s#)
             ((== 'oil x) s#)
             (else u#))
           => '(extra olive))

(check-run* (r)
            (fresh (x y)
                   (== 'split x)
                   (== 'pea y)
                   (== (cons x (cons y '())) r))
            => '((split pea)))

(check-run* (r)
            (fresh (x y)
                   (conde
                     ((== 'split x) (== 'pea y))
                     ((== 'navy x) (== 'bean y))
                     (else u#))
                   (== (cons x (cons y '())) r))
            => '((split pea) (navy bean)))

(check-run* (r)
            (fresh (x y)
                   (conde
                     ((== 'split x) (== 'pea y))
                     ((== 'navy x) (== 'bean y))
                     (else u#))
                   (== (cons x (cons y (cons 'soup '()))) r))
            => '((split pea soup) (navy bean soup)))

(define (teacupo x)
  (conde
    ((== 'tea x) s#)
    ((== 'cup x) s#)
    (else u#)))

(check-run* (x)
            (teacupo x)
            => '(tea cup))

(check-run* (r)
            (fresh (x y)
                   (conde
                     ((teacupo x) (== #t y) s#)
                     ((== #f x) (== #t y))
                     (else u#))
                   (== (cons x (cons y '())) r))
            => '((#f #t) (tea #t) (cup #t))) ; not sure why ordering is different than in book

(check-run* (r)
            (fresh (x y z)
                   (conde
                     ((== y x) (fresh (x) (== z x)))
                     ((fresh (x) (== y x)) (== z x))
                     (else u#))
                   (== (cons y (cons z '())) r))
            => '((_.0 _.1) (_.0 _.1)))

(check-run* (r)
            (fresh (x y z)
                   (conde
                     ((== y x) (fresh (x) (== z x)))
                     ((fresh (x) (== y x)) (== z x))
                     (else u#))
                   (== #f x)
                   (== (cons y (cons z '())) r))
            => '((#f _.0) (_.0 #f)))

(check-run* (q)
            (let ((a (== #t q))
                  (b (== #f q)))
              b)
            => '(#f))

(check-run* (q)
            (let ((a (== #t q))
                  (b (fresh (x)
                            (== x q)
                            (== #f x)))
                  (c (conde
                       ((== #t q) s#)
                       (else (== #f q)))))
              b)
            => '(#f))
