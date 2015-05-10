#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

[check-equal? (run* (q)
                   u#)
              '()]

[check-equal? (run* (q)
                    (== #t q))
              '(#t)]

[check-equal? (run* (q)
                    u#
                    (== #t q))
              '()]

[check-equal? (run* (q)
                    s#
                    (== #t q))
              '(#t)]

[check-equal? (run* (r)
                    s#
                    (== 'corn r))
              '(corn)]

[check-equal? (run* (r)
                    u#
                    (== 'corn r))
              '()]

[check-equal? (run* (q)
                    s#
                    (== #f q))
              '(#f)]

[check-equal? (run* (x)
                    (let ((x #t))
                      (== #f x)))
              '()]

[check-equal? (run* (q)
                    (let ((x #f))
                      (== #f x)))
              '(_.0)]

[check-equal? (run* (x)
                    (let ((x #f))
                      (== #t x)))
              '()]

[check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== #t q)))
              '(#t)]

[check-equal? (run* (q)
                    (fresh (x)
                           (== x #t)
                           (== #t q)))
              '(#t)]

[check-equal? (run* (q)
                    (fresh (x)
                           (== x #t)
                           (== q #t)))
              '(#t)]

[check-equal? (run* (x) s#)
              '(_.0)]

;; First example on page 8. WTF?!?!
[check-equal? (run* (x)
                    (let ((x #f))
                      (fresh (x)
                             (== #t x))))
              '(_.0)]

[check-equal? (run* (r)
                   (fresh (x y)
                          (== (cons x (cons y '())) r)))
              '((_.0 _.1))]


[check-equal? (run* (s)
                    (fresh (t u)
                           (== (cons t (cons u '())) s)))
              '((_.0 _.1))]


[check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons y (cons x (cons y '()))) r)))))
              '((_.0 _.1 _.0))]

[check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons x (cons y (cons x '()))) r)))))
              '((_.0 _.1 _.0))]

[check-equal? (run* (q)
                    (== #f q)
                    (== #t q))
              '()]


[check-equal? (run* (q)
                    (== #f q)
                    (== #f q))
              '(#f)]


[check-equal? (run* (q)
                    (let ((x q))
                      (== #t x)))
              '(#t)]


[check-equal? (run* (r)
                    (fresh (x)
                           (== x r)))
              '(_.0)]


[check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== x q)))
              '(#t)]


[check-equal? (run* (q)
                    (fresh (x)
                           (== x q)
                           (== #t x)))
              '(#t)]


[check-false (cond
               (#f #t)
               (else #f))]


[check-equal? (run* (x)
                    (conde
                     ((== 'olive x) s#)
                     ((== 'oil x) s#)
                     (else u#)))
              '(olive oil)]


[check-equal? (run 1 (x)
                   (conde
                    ((== 'olive x) s#)
                    ((== 'oil x) s#)
                    (else u#)))
              '(olive)]

[check-equal? (run* (x)
                    (conde
                     ((== 'virgin x) u#)
                     ((== 'olive x) s#)
                     (s# s#)
                     ((== 'oil x) s#)
                     (else u#)))
              '(olive _.0 oil)]

[check-equal? (run 2 (x)
                   (conde
                    ((== 'extra x) s#)
                    ((== 'virgin x) u#)
                    ((== 'olive x) s#)
                    ((== 'oil x) s#)
                    (else u#)))
              '(extra olive)]

[check-equal? (run* (r)
                    (fresh (x y)
                           (== 'split x)
                           (== 'pea y)
                           (== (cons x (cons y '())) r)))
              '((split pea))]

[check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                            ((== 'split x) (== 'pea y))
                            ((== 'navy x) (== 'bean y))
                            (else u#))
                           (== (cons x (cons y '())) r)))
              '((split pea) (navy bean))]

[check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                            ((== 'split x) (== 'pea y))
                            ((== 'navy x) (== 'bean y))
                            (else u#))
                           (== (cons x (cons y (cons 'soup '()))) r)))
              '((split pea soup) (navy bean soup))]

(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) s#)
     ((== 'cup x) s#))))

[check-equal? (run* (x)
                   (teacupo x))
              '(tea cup)]


;; Review from here down

[check-equal? (run* (r)
                    (fresh (x y)
                           (conde
                            ((teacupo x) (== #t y) s#)
                            ((== #f x) (== #t y))
                            (else u#))
                           (== (cons x (cons y '())) r)))
              '((tea #t)  (cup #t) (#f #t))]
;; teacupo has two solutions tea & cup 


;; HALP!! I'm fuzzy on how this works
[check-equal? (run* (r)
                    (fresh (x y z)
                           (conde
                            ((== y x) (fresh (x) (== z x)))
                            ((fresh (x) (== y x)) (== z x))
                            (else u#))
                           (== (cons y (cons z '())) r)))
              '((_.0 _.1) (_.0 _.1))]

[check-equal? (run* (r)
                    (fresh (x y z)
                           (conde 
                            ((== y x) (fresh (x) (== z x)))
                            ((fresh (x) (== y x)) (== z x))
                            (else u#))
                           (== #f x)
                           (== (cons y (cons z '())) r)))
              '((#f _.0) (_.0 #f))]
;; Narrative walk through:
;; First conde line:
;;   Unify y and x they're both "some value" y == x == _.0
;;   Create a new x, x' which is now "some value" _.1
;;   Unify x' with z, so x' == z == _.1
;; (== #f x) line
;;   y and x which were _.0 are now #f
;; First answer is (#f _.0) because numbering of "some values" don't matter
;; 
;; Second conde line:
;;   Create a new x, x' which is now "some value" _.0
;;   Unify x' with y, so x' == y == _.0
;;   Unify x and z. x == z == _.1
;; (== #f x) line
;;   x == z == #f 
;; Second answer is (_.0 #f)
;; Overall answer is ((#f _.0) (_.0 #f)) in some order

[check-equal? (run* (q)
                    (let ((a (== #t q))
                          (b (== #f q)))
                      b))
              '(#f)]

[check-equal? (run* (q)
                    (let ((a (== #t q))
                          (b (fresh (x)
                                    (== x q)
                                    (== #f x)))
                          (c (conde
                              ((== #t q) s#)
                              ((#t) (== #f q))
                              (else u#))))
                      b))
              '(#f)]
