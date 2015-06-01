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

; # 19 - 42
; numbers are lists of 0 1
; the first bit tells us if it is even or odd
; they all end with a 1
; 0 is () empty list
; (0) means nothing
; (a b c) => a2^0 + b2^1 + c2^2
; (0 . n) is twice n
; (1 . n) is twice n + 1

(define numb_me
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (+ (car l)
               (* (numb_me (cdr l)) 2))])))
(module+ test
  [check-equal? (run* (s) (== (numb_me '()) s)) 0]
  [check-equal? (run* (s) (== (numb_me '(1)) s)) 1]
  [check-equal? (run* (s) (== (numb_me '(1 0 1 1)) s)) 13]
  [check-equal? (run* (s) (== (numb_me '(0 1 0 1 0 0 0 1 1 1 0 0 0 0 1)) s))
                17290])

; # 43 do build-num a.k.a biteme
(define bit_it
  (lambda (n)
    (cond
      [(zero? n) '()]
      [(and (not (zero? n)) (even? n))
       (cons 0 (bit_it (/ n 2)))]
      [(odd? n) (cons 1 (bit_it (/ (- n 1) 2)))])))

[check-equal?  (run* (s) (== s (bit_it 13))) '((1 0 1 1))]
; honestly, it would have taken me forever to come up with that def

; # 44 - 46 the same but different
(define bite_me
  (lambda (n)
    (cond
      [(odd? n) (cons 1 (bite_me (/ (- n 1) 2)))]
      [(and (not (zero? n)) (even? n))
       (cons 0 (bite_me (/ n 2)))]
      [(zero? n) '()])))

[check-equal?  (run* (s) (== s (bite_me 13))) '((1 0 1 1))]
; interestingly enough, only one condition will be true in each pass
; therefore, the order of the conditions is irrelevant

; # 47 -  52
(define suma
  (lambda (l1 l2)
    (bite_me (+ (numb_me l1) (numb_me l2)))))

(module+ test
  [check-equal? (run* (s) (== (suma '(1) '(1)) s)) '(0 1)]
  [check-equal? (run* (s) (== (suma '(0 0 0 1) '(1 1 1)) s)) '(1 1 1 1)]
  [check-equal? (run* (s) (== (suma '(1 1 1) '(0 0 0 1)) s)) '(1 1 1 1)]
  [check-equal? (run* (s) (== (suma '() '(1 1 0 0 1)) s)) '(1 1 0 0 1)]
  [check-equal? (run* (s) (== (suma '(1 1 0 0 1) '(1)) s)) '(0 0 0 1 1)])

