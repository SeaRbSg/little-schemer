#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define bit-xoro
  (lambda (x y r)
    (conde
      [(== 0 x) (== 0 y) (== 0 r)]
      [(== 1 x) (== 0 y) (== 1 r)]
      [(== 0 x) (== 1 y) (== 1 r)]
      [(== 1 x) (== 1 y) (== 0 r)]
      [else %u])))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 0)
    (== `(,x ,y) s)))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 1)
    (== `(,x ,y) s)))


(run* (s) ;; truth-table
  (fresh (x y r)
    (bit-xoro x y r)
    (== `(,x ,y ,r) s)))

(define bit-ando
  (lambda (x y r)
    (conde
      [(== 0 x) (== 0 y) (== 0 r)]
      [(== 1 x) (== 0 y) (== 0 r)]
      [(== 0 x) (== 1 y) (== 0 r)]
      [(== 1 x) (== 1 y) (== 1 r)]
      [else %u])))

(define half-addero
  (lambda (x y r c)
    (all
      (bit-xoro x y r)
      (bit-ando x y c))))

(run* (r)
  (half-addero 1 1 r 1))

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

(run* (s)
  (fresh (r c)
    (full-addero 1 1 1 r c)
    (== `(,r ,c) s)))

;; Each number is represented uniquely, therefore '(0) cannot
;; also represent the number 0. What does this mean?


(define build-num
  (lambda (n)
    (cond
      [(zero? n) '()]
      [(even? n) (cons 0 (build-num (quotient n 2)))]
      [else (cons 1 (build-num (quotient (- n 1) 2)))])))


(define poso ;; positive-o
  (lambda (n)
    (fresh (a d)
      (== (a . d) n))))


(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== (a ad . dd) n))))
