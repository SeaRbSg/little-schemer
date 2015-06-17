#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")


; # 5
(define bitcho  ; output is 1 if different inputs
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
      (bitcho x y 0)
      (conso x `(,y) q)))
  '((0 0) (1 1))]

; r==1 if x, y == 1, 0 || 0, 1 (different)
[check-equal?
  (run* (q)
    (fresh (x y)
      (bitcho x y 1)
      (conso x `(,y) q)))
  '((1 0) (0 1))]

; # 9
[check-equal?
  (run* (s)
    (fresh (x y r)
      (bitcho x y r)
      (== `(,x ,y ,r) s)))
  '((0 0 0) (1 0 1) (0 1 1) (1 1 0))]
; yep, it solves for all possible values (conde)

; # 10 - 11
(define bitano ; output 1 only if all inputs 1
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

; The idea is to understand addition of bit numbers
;            0     0     1     1
;           +0    +1    +0    +1
;          ----  ----  ----  ----
; result:    0     1     1     0   <=== bitcho !!
;
; carry:     0     0     0     1   <=== bitano !!
;
; total:    (00)  (10)  (10)  (01) <=== (r c) remember for later !!
; total:    ()    (1)   (1)   (01)      better this way, 0 not allowed at end

; # 12
(define halfaddero       ; columnar / vertical / bitsy addition
  (lambda (x y r c)      ; given two bit_numbers x y, get result and carry
    (all
      (bitcho x y r)     ; x + y => sum r
      (bitano x y c))))  ; x + y => and I carry c

[check-equal?
  (run* (r)
    (halfaddero 1 1 r 1))
  '(0)]
; bitcho 1 1 r => r == 0
; bitano 1 1 1 succeeds, all succeed, ==> r == 0

; # 13 - 14
[check-equal?
  (run* (s)
    (fresh (x y r c)
      (halfaddero x y r c)
      (== `(,x ,y ,r ,c) s)))
  '((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))]
; all possible values again
; halfaddero x + y = r + (2c)

; # 15 - 16
(define fulladdero            ; halfaddero with an already existing carry (b)
  (lambda (b x y r c)         ; i.e. adding bits in the middle of a big sum
    (fresh (r1 c1 c2)
      (halfaddero x y r1 c1)  ; add the bits
      (halfaddero r1 b r c2)  ; add the carrys
      (bitcho c1 c2 c))))     ; add the carrys

; fulladdero takes a columns where (for example) x:1 y:0 b:1
; and comes up with a result r:0 and a new carry c:1 (new b)
;              |
;              V
;            0 1 1 1  carry bits (b) <part of the process/output/input>
;          0 1 0 1    number 1 (x1 x2 x3 x4)
;          0 1 1 1    number 2 (y1 y2 y3 y4)
;        ----------
; sum:     0 0 0 1 1  the result <output>
;
; So halfaddero works for the first or rightmost bit of a number/list of bits
; while fulladdero works for any position/column bit (because it considers the
; ongoing carry bit)

[check-equal?
  (run* (s)
    (fresh (r c)
      (fulladdero 0 1 1 r c)
      (== `(,r ,c) s)))
  '((0 1))]

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
(define beat_it
  (lambda (n)
    (cond
      [(zero? n) '()]
      [(and (not (zero? n)) (even? n))
       (cons 0 (beat_it (/ n 2)))]
      [(odd? n) (cons 1 (beat_it (/ (- n 1) 2)))])))

[check-equal?  (run* (s) (== s (beat_it 13))) '((1 0 1 1))]
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
(define sum_bits
  (lambda (l1 l2)
    (bite_me (+ (numb_me l1) (numb_me l2)))))

(module+ test
  [check-equal? (run* (s) (== (sum_bits '(1) '(1)) s)) '(0 1)]
  [check-equal? (run* (s) (== (sum_bits '(0 0 0 1) '(1 1 1)) s)) '(1 1 1 1)]
  [check-equal? (run* (s) (== (sum_bits '(1 1 1) '(0 0 0 1)) s)) '(1 1 1 1)]
  [check-equal? (run* (s) (== (sum_bits '() '(1 1 0 0 1)) s)) '(1 1 0 0 1)]
  [check-equal? (run* (s) (== (sum_bits '(1 1 0 0 1) '(1)) s)) '(0 0 0 1 1)])

;# 80 - 84
(define poso
  (lambda (n)
    (pairo n)))             ; the simplest way
    ; (fresh (a d)
      ; (conso a d n))))    ; this one also works (and preferred for me)
      ; (== `(,a . ,d) n))))

[check-equal? (run* (q) (poso '(0 0 1)) (== #t q)) '(#t)]
[check-equal? (run* (q) (poso '(1)) (== #t q))     '(#t)]
[check-equal? (run* (q) (poso '(a b c)) (== #t q)) '(#t)]
[check-equal? (run* (q) (poso '()) (== #t q))      '()]

[check-equal? (run* (r) (poso r)) '((_.0 . _.1))]

; # 85 - 91
(define >1o
  (lambda (n)
    (fresh (a aa dd)
      (== `(,a ,aa . ,dd) n))))

[check-equal? (run* (q) (>1o '(0 1 1)) (== #t q)) '(#t)]
[check-equal? (run* (q) (>1o '(1))     (== #t q)) '()]
[check-equal? (run* (q) (>1o '())      (== #t q)) '()]
[check-equal? (run* (r) (>1o r)) '((_.0 _.1 . _.2))]

; # 118
; so now that fulladdero can take on adding columnar bits we'll try to add
; whole numbers (lists of bits)
; given 2 grown_up numbers (lists of bits) n, m
(define addero
  (lambda (d n m r)
    (condi
      [(== 0 d) (nullo m) (== n r)]               ; why not poso n?
      [(== 0 d) (nullo n) (== m r) (poso m)]

      [(== 1 d) (nullo m) (addero 0 n '(1) r)]    ; again, why not poso n?
      [(== 1 d) (nullo n)
       (poso m)
       (addero 0 '(1) m r)]

      [(== '(1) n) (== '(1) m)
       (fresh (a c)
         (== `(,a ,c) r)                          ; remember from before
         (fulladdero d 1 1 a c))]                 ; d'oh! dude!

      [(== '(1) n) (gen-addero d n m r)]
      [(== '(1) m) (>1o n) (>1o r)
         (addero d '(1) n r)]

      [(>1o n) (gen-addero d n m r)]
      [u#])))

; # 118
(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)           ; \
      (== `(,b . ,y) m) (poso y)  ;  > Extract the car bits a b c
      (== `(,c . ,z) r) (poso z)  ; /
      (alli
        (fulladdero d a b c e)    ; a b are numbers, c is result, e the carry
        (addero e x y z)))))      ; the e becomes the d as we recurr

[check-equal? (run* (q) (gen-addero 0 '(0 1 0 1) '(0 1 1 1) q)) '((0 0 0 1 1))]
; carry bits              0 1 1 1
; n (x1 x2 x3 x4)       0 1 0 1
; m (y1 y2 y3 y4)       0 1 1 1
;                      ----------
; sum:                  0 0 0 1 1

; as we start with n (0 1 0 1) and m (0 1 1 1) => a:0 b:0 (d:0 obviously)
; fulladdero d:0 a:0 b:0 c e => c:0 (so r:(0...)) and e (carry) is 0
; addero new_carry:e:0 cdr_n:(1 0 1) cdr_m:(1 1 1) cdr_result:(...)
; as we go on we build the result, left to right, from car to cdr,
; carrying the carry-bit along
[check-equal? (run* (q) (addero 0 '(0 1 0 1) '(0 1 1 1) q)) '((0 0 0 1 1))]

; # 120 -125
[check-equal?
  (run* (s) (gen-addero 1 '(0 1 1) '(1 1) s))
  '((0 1 0 1))]

; $ 126 - 127
[check-equal?
  (run* (s)
    (fresh (x y)
      (addero 0 x y '(1 0 1))
      (== `(,x ,y) s)))
  '(((1 0 1) ())
    (() (1 0 1))
    ((1) (0 0 1))
    ((0 0 1) (1))
    ((1 1) (0 1))
    ((0 1) (1 1)))]

; # 128 - 129
(define +o
  (lambda (n m k)
    (addero 0 n m k)))

[check-equal?
  (run* (s)
    (fresh (x y)
      (+o x y '(1 0 1))
      (== `(,x ,y) s)))
  '(((1 0 1) ())
    (() (1 0 1))
    ((1) (0 0 1))
    ((0 0 1) (1))
    ((1 1) (0 1))
    ((0 1) (1 1)))]

; # 130 - 133
(define -o
  (lambda (n m k)
    (+o m k n)))

(module+ test
  [check-equal?  (run* (q) (-o '(0 0 0 1) '(1 0 1) q)) '((1 1))]
  [check-equal?  (run* (q) (-o '(0 1 1) '(0 1 1) q)) '(())]
  [check-equal?  (run* (q) (-o '(0 1 1) '(0 0 0 1) q)) '()])
