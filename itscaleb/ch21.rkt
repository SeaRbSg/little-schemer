#lang racket
(require "reasoned.rkt")
(require minikanren)
(require rackunit)

(check-equal? (run* (q)
                    u#)
              '())

(check-equal? (run* (q)
                   (conde
                    (u# s#)))
              '())

(check-equal? (run* (q)
                    (conde
                     (s# s#)))
              '(_.0))

(check-equal? (run* (q)
                    (conde
                     ((== 'olive q) s#)
                     ((== 'oil q) s#)))
              '(olive oil))

(check-equal? (run* (q)
                    (conde
                     ((== 'olive q) s#)
                     (s# s#)
                     ((== 'oil q) s#)))
              '(olive _.0 oil))

(check-equal? (run* (q)
                    (fresh (x y)
                           (== 'split x)
                           (== 'pea y)
                           (== (cons x (cons y '())) q)))
              '((split pea)))

(check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                            ((== 'split x) (== 'pea y))
                            ((== 'navy x) (== 'bean y)))
                           (== (cons x (cons y (cons 'soup'()))) r)))
              ; seems like this line ^ evaluates for each conde goal combination
              '((split pea soup) (navy bean soup)))

(define teacup0
  (lambda (x)
    (conde
     ((== 'tea x) s#)
     ((== 'cup x) s#))))

(check-equal? (run* (x) (teacup0 x))
              '(tea cup))

(check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                            ((teacup0 x) (== #t y) s#)
                            ((== #f x) (== #t y)))
                            ; The nested conde loop is tricky
                           (== (cons x (cons y '())) r)))
              '((#f #t) (tea #t) (cup #t)))

(check-equal? (run* (r)
                    (fresh (x y z)
                           (conde
                            ((== y x) (fresh (x) (== z x)))
                            ((fresh (x) (== y x)) (== z x)))
                           (== #f x)
                           (== (cons y (cons z '())) r)))
              '((#f _.0) (_.0 #f)))
