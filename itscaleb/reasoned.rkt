#lang racket
(require "../lib/mk.rkt")

(define s# (== #f #f))

(define u# (== #f #t))

(define caro
  (lambda (p a)
    (fresh (d)
           (conso a d p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (conso a d p))))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(provide s# u# caro cdro pairo conso nullo eqo)
