#lang racket
(require "../lib/mk.rkt")

(provide s# u# caro cdro conso nullo eqo pairo)

(define s# (== #f #f)) ;; succeed
(define u# (== #f #t)) ;; fail

(define caro
  (lambda (list r)
    (fresh (x)
           (== (cons r x) list))))

(define cdro
  (lambda (list r)
    (fresh (x)
           (== (cons x r) list))))

(define conso
  (lambda (l1 l2 r)
    (== (cons l1 l2) r)))

(define nullo
  (lambda (l)
    (== l '())))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (list)
    (fresh (x y)
           (conso x y list))))
