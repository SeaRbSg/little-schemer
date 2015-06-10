#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

;; 5
;; I like building up from nand :]
(define (bit-nando x y r)
  (conde
    [(== 0 x) (== 0 y) (== 1 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]
    [else u#]))

(define (bit-xoro x y r)
  (fresh (s t u)
    (bit-nando x y s)
    (bit-nando x s t)
    (bit-nando s y u)
    (bit-nando t u r)))

;; 6
(check-run* (s)
            (fresh (x y)
              (bit-xoro x y 0)
              (== `(,x ,y) s))
            => '((0 0)
                 (1 1)))

;; 8
(check-run* (s)
            (fresh (x y)
              (bit-xoro x y 1)
              (== `(,x ,y) s))
            => '((1 0)
                 (0 1)))

;; 9
(check-run* (s)
            (fresh (x y r)
              (bit-xoro x y r)
              (== `(,x ,y ,r) s))
            => '((0 0 0)
                 (1 0 1)
                 (0 1 1)
                 (1 1 0)))

;; 10
(define (bit-noto x r)
  (bit-nando x x r))

(define (bit-ando x y r)
  (fresh (s)
    (bit-nando x y s)
    (bit-noto s r)))

;; 11
(check-run* (s)
            (fresh (x y r)
              (bit-ando x y 1)
              (== `(,x ,y) s))
            => '((1 1)))

;; 12
(define (half-addero x y r c)
  (all
    (bit-xoro x y r)
    (bit-ando x y c)))

(check-run* (r)
            (half-addero 1 1 r 1)
            => '(0))

;; 13
(check-run* (s)
            (fresh (x y r c)
              (half-addero x y r c)
              (== `(,x ,y ,r ,c) s))
            => '((0 0 0 0)
                 (1 0 1 0)
                 (0 1 1 0)
                 (1 1 0 1)))

;; 14
;; it's a half-bit adder that adds x to y with a
;; carryover bit c and a carryout bit r

;; 15
(define (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(check-run* (s)
            (fresh (r c)
              (full-addero 0 1 1 r c)
              (== `(,r ,c) s))
            => '((0 1)))

;; 16
(check-run* (s)
            (fresh (r c)
              (full-addero 1 1 1 r c)
              (== `(,r ,c) s))
            => '((1 1)))

;; 17
(check-run* (s)
            (fresh (b x y r c)
              (full-addero b x y r c)
              (== `(,b ,x ,y ,r ,c) s))
            => '((0 0 0 0 0)
                 (1 0 0 1 0)
                 (0 1 0 1 0)
                 (1 1 0 0 1)
                 (0 0 1 1 0)
                 (1 0 1 0 1)
                 (0 1 1 0 1)
                 (1 1 1 1 1)))
;; 18
;; b + x + y = r with a carry out bit of c

;; 40
(check-equal? (build-num 0) '())

;; 41
(check-equal? (build-num 36) '(0 0 1 0 0 1))

;; 42
(check-equal? (build-num 19) '(1 1 0 0 1))
