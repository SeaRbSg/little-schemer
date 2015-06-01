#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

; # 5
(define bitchocho
  (lambda (x y r)
    (conde
      ((== 0 x)(== 0 y)(== 0 r))
      ((== 1 x)(== 0 y)(== 1 r))
      ((== 0 x)(== 1 y)(== 1 r))
      ((== 1 x)(== 1 y)(== 0 r))
      (u#))))

; # 5 - 8
; r==0 in two cases: x, y == 0, 0 || 1, 1 (same)
[check-equal?
  (run* (q)
    (fresh (x y)
      (bitchocho x y 0)
      (conso x `(,y) q)))
  '((0 0) (1 1))]

; r==1 if x, y == 1, 0 || 0, 1 (different)
[check-equal?
  (run* (q)
    (fresh (x y)
      (bitchocho x y 1)
      (conso x `(,y) q)))
  '((1 0) (0 1))]

; # 9
[check-equal?
  (run* (s)
    (fresh (x y r)
      (bitchocho x y r)
      (== `(,x ,y ,r) s)))
  '((0 0 0) (1 0 1) (0 1 1) (1 1 0))]
; yep, it solves for all possible values (conde)

; # 10 - 11
(define bitano
  (lambda (x y r)
    (conde
      ((== 0 x)(== 0 y)(== 0 r))
      ((== 1 x)(== 0 y)(== 0 r))
      ((== 0 x)(== 1 y)(== 0 r))
      ((== 1 x)(== 1 y)(== 1 r))
      (u#))))

[check-equal?
  (run* (q)
    (fresh (x y)
      (bitano x y 1)
      (conso x `(,y) q)))
  '((1 1))]

; # 12
(define halfaddero
  (lambda (x y r c)
    (all
      (bitchocho x y r) ; given 2 I know the other. 1 eq 3 var
      (bitano x y c)))) ; given 2 I know the other. 1 eq 3 var
; halfaddero because of all is 2 eq with 4 var

[check-equal?
  (run* (r)
    (halfaddero 1 1 r 1))
  '(0)]
; bitchocho 1 1 r => r == 0
; bitano 1 1 1 succeeds, all succeed, ==> r == 0

; # 13 - 14
[check-equal?
  (run* (s)
    (fresh (x y r c)
      (halfaddero x y r c)
      (== `(,x ,y ,r ,c) s)))
  '((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))]
; all possible values again
; halfaddero x + y = r + (2c) BUT it is NOT 1 eq 4 var. It is 2 eq!!

; # 15 - 16
(define fulladdero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (halfaddero x y w xy)   ; 2 eq
      (halfaddero w b r wz)   ; 2 eq
      (bitchocho xy wz c))))  ; 1 eq
; 5 eq 8 var

[check-equal?
  (run* (s)
    (fresh (r c)
      (fulladdero 0 1 1 r c)
      (== `(,r ,c) s)))
  '((0 1))]
; we are solving 5 eq with 3 vars known ==> 5 var unknows
; there should be a single solution

[check-equal?
  (run* (s)
    (fresh (r c)
      (fulladdero 1 1 1 r c)
      (== `(,r ,c) s)))
  '((1 1))]

; # 17 - 18
[check-equal?
  (run* (s)
    (fresh (b x y r c)
      (fulladdero b x y r c)
      (== `(,b ,x ,y ,r ,c) s)))
  '((0 0 0 0 0)
    (1 0 0 1 0)
    (0 1 0 1 0)
    (1 1 0 0 1)
    (0 0 1 1 0)
    (1 0 1 0 1)
    (0 1 1 0 1)
    (1 1 1 1 1))]
; fulladdero solves b + x + y = r + (2c)


