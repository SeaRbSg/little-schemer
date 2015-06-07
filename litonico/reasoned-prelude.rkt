#lang racket

(provide %u %s eq-car? caro cdro conso nullo pairo eqo teacupo)
(require "../lib/mk.rkt")

(define %s (== #f #f))
(define %u (== #f #t))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define caro
  (lambda (lst head)
    (fresh (tail)
      (== (cons head tail) lst))))

(define cdro
  (lambda (lst tail)
    (fresh (head)
      (== (cons head tail) lst))))

(define conso
  (lambda (head tail lst)
    (== (cons head tail) lst)))

(define nullo
  (lambda (lst)
    (== '() lst)))

(define eqo ==)

(define pairo
  (lambda (pair)
    (fresh (head tail)
      (conso head tail pair))))

(define teacupo
  (lambda (x)
    (conde
      [(== 'tea x) %s]
      [(== 'cup x) %s]
      [%u])))
