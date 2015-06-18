#lang racket
(require "../../basic_defs.rkt")
(require "../../lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../../../lib/mk.rkt")

(provide nando noto ando oro xoro muxo dmuxo not16o and16o or16o mux16o or8Wayo mux4Way16o mux8Way16o dmux4Wayo dmux8Wayo)

; basic building block NAND

(define nando
  (lambda (a b out)
    (conde
      [(== 0 a) (== 0 b) (== 1 out)]
      [(== 0 a) (== 1 b) (== 1 out)]
      [(== 1 a) (== 0 b) (== 1 out)]
      [(== 1 a) (== 1 b) (== 0 out)])))

; Most basic logic gates derived from nand

(define noto
  (lambda (a out)
    (nando a a out)))

[check-equal?  (run* (q) (noto 1 q)) '(0)]
[check-equal?  (run* (q) (noto 0 q)) '(1)]

(define ando
  (lambda (a b out)
    (fresh (x)
      (nando a b x)
      (noto x out))))

[check-equal?  (run* (q) (ando 0 0 q)) '(0)]
[check-equal?  (run* (q) (ando 0 1 q)) '(0)]
[check-equal?  (run* (q) (ando 1 0 q)) '(0)]
[check-equal?  (run* (q) (ando 1 1 q)) '(1)]

(define oro
  (lambda (a b out)
    (fresh (x y)
      (noto a x)
      (noto b y)
      (nando x y out))))

[check-equal? (run* (q) (oro 0 0 q)) '(0)]
[check-equal? (run* (q) (oro 0 1 q)) '(1)]
[check-equal? (run* (q) (oro 1 0 q)) '(1)]
[check-equal? (run* (q) (oro 1 1 q)) '(1)]

; Basic logic gates based on above

(define xoro
  (lambda (a b out)
    (fresh (x y w z)
      (noto a x)
      (noto b y)
      (ando a y w)
      (ando b x z)
      (oro w z out))))

[check-equal? (run* (q) (xoro 0 0 q)) '(0)]
[check-equal? (run* (q) (xoro 0 1 q)) '(1)]
[check-equal? (run* (q) (xoro 1 0 q)) '(1)]
[check-equal? (run* (q) (xoro 1 1 q)) '(0)]

(define muxo ; multiplexor
  (lambda (a b sel out)
    (conde
      [(== 0 sel) (== a out)]
      [(== 1 sel) (== b out)])))

[check-equal? (run* (q) (muxo 0 0 0 q)) '(0)]
[check-equal? (run* (q) (muxo 0 1 0 q)) '(0)]
[check-equal? (run* (q) (muxo 1 0 0 q)) '(1)]
[check-equal? (run* (q) (muxo 1 1 0 q)) '(1)]
[check-equal? (run* (q) (muxo 0 0 1 q)) '(0)]
[check-equal? (run* (q) (muxo 0 1 1 q)) '(1)]
[check-equal? (run* (q) (muxo 1 0 1 q)) '(0)]
[check-equal? (run* (q) (muxo 1 1 1 q)) '(1)]

(define dmuxo ; demultiplexor
  (lambda (in sel a b)
    (conde
      [(== 0 sel) (== in a) (== 0  b)]
      [(== 1 sel) (== 0  a) (== in b)])))

[check-equal? (run* (q) (fresh (a b) (dmuxo 0 0 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 0 1 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 1 0 a b) (== `(,a ,b) q))) '((1 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 1 1 a b) (== `(,a ,b) q))) '((0 1))]

; n variants like 16bit variants

(define not16o
  (lambda (a out)
    (conde
      [(nullo a) (nullo out)]
      [(fresh (ac ad oc od)
         (conso ac ad a)
         (conso oc od out)
         (noto ac oc)
         (not16o ad od))])))

[check-equal? (run* (q) (not16o '(1 1 0 1 0) q)) '((0 0 1 0 1))]
[check-equal? (run* (q) (not16o '(0 0 0 1 0) q)) '((1 1 1 0 1))]
[check-equal? (run* (q) (not16o '(1) q)) '((0))]
[check-equal? (run* (q) (not16o '(0 0) q)) '((1 1))]

(define and16o ; and for n buses
  (lambda (a b out)
    (conde
      [(nullo a) (nullo b) (nullo out)]
      [(fresh (ac ad bc bd oc od)
         (conso ac ad a)
         (conso bc bd b)
         (conso oc od out)
         (ando ac bc oc)
         (and16o ad bd od))])))

[check-equal?  (run* (q) (and16o '(0 0 0 1) '(1 1 1 1) q)) '((0 0 0 1))]
[check-equal?  (run* (q) (and16o '(1 1 1 1) '(1 1 1 1) q)) '((1 1 1 1))]
[check-equal?  (run* (q) (and16o '(0 0 0 0) '(1 1 1 1) q)) '((0 0 0 0))]
[check-equal?  (run* (q) (and16o '(0 0 0) '(1 1 1 1) q)) '()]
[check-equal?  (run* (q) (and16o '() '(1 1 1 1) q)) '()]

(define or16o
  (lambda (a b out)
    (conde
      [(nullo a) (nullo b) (nullo out)]
      [(fresh (ac ad bc bd oc od)
         (conso ac ad a)
         (conso bc bd b)
         (conso oc od out)
         (oro ac bc oc)
         (or16o ad bd od))])))

[check-equal?  (run* (q) (or16o '(0 0 0 0 0) '(0 0 0 0 0) q)) '((0 0 0 0 0))]
[check-equal?  (run* (q) (or16o '(0 0 0 0 0) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (or16o '(1 1 1 1 1) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (or16o '(1 0 1 0 1) '(0 1 0 1 0) q)) '((1 1 1 1 1))]

(define mux16o
  (lambda (a b sel out)
      (conde
        [(== 0 sel) (== a out)]
        [(== 1 sel) (== b out)])))

[check-equal?  (run* (q) (mux16o '(1 0 1 0 1) '(0 1 0 1 0) 0 q)) '((1 0 1 0 1))]
[check-equal?  (run* (q) (mux16o '(1 0 1 0 1) '(0 1 0 1 0) 1 q)) '((0 1 0 1 0))]

(define or8Wayo
  (lambda (a out)
      (conde
        [(nullo a) (== 0 out)]
        [(fresh (ac ad)
           (conso ac ad a)
           (conde
             [(== 1 ac) (== 1 out)]
             [(== 0 ac) (or8Wayo ad out)]))])))

[check-equal? (run* (q) (or8Wayo '(0 0 0 0 0 0 0 0) q)) '(0)]
[check-equal? (run* (q) (or8Wayo '(1 0 1 0 1 0 1 0) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(1 1 1 1 1 1 1 1) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(0 0 0 0 1 0 0 0) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(0 0 0 0 0 0 0 1) q)) '(1)]

(define mux4Way16o ; 4 way multiplexor
  (lambda (a b c d sel out)
    (fresh (s0 s1 sd)
      (== `(,s0 ,s1 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== a out)]
        [(== 0 s0) (== 1 s1) (== b out)]
        [(== 1 s0) (== 0 s1) (== c out)]
        [(== 1 s0) (== 1 s1) (== d out)]))))

[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(0 0) q)) '((0 0))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(0 1) q)) '((0 1))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(1 0) q)) '((1 0))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(1 1) q)) '((1 1))]

(define mux8Way16o ; 8 way multiplexor
  (lambda (a b c d e f g h sel out)
    (fresh (s0 s1 s2 sd)
      (== `(,s0 ,s1 ,s2 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== 0 s2) (== a out)]
        [(== 0 s0) (== 0 s1) (== 1 s2) (== b out)]
        [(== 0 s0) (== 1 s1) (== 0 s2) (== c out)]
        [(== 0 s0) (== 1 s1) (== 1 s2) (== d out)]
        [(== 1 s0) (== 0 s1) (== 0 s2) (== e out)]
        [(== 1 s0) (== 0 s1) (== 1 s2) (== f out)]
        [(== 1 s0) (== 1 s1) (== 0 s2) (== g out)]
        [(== 1 s0) (== 1 s1) (== 1 s2) (== h out)]))))

[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 0 0) q)) '(a)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 0 1) q)) '(b)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 1 0) q)) '(c)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 1 1) q)) '(d)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 0 0) q)) '(e)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 0 1) q)) '(f)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 1 0) q)) '(g)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 1 1) q)) '(h)]

(define dmux4Wayo ; demultiplexor
  (lambda (in sel a b c d)
    (fresh (s0 s1 sd)
      (== `(,s0 ,s1 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== in a) (== 0 b) (== 0 c) (== 0 d)]
        [(== 0 s0) (== 1 s1) (== 0 a) (== in b) (== 0 c) (== 0 d)]
        [(== 1 s0) (== 0 s1) (== 0 a) (== 0 b) (== in c) (== 0 d)]
        [(== 1 s0) (== 1 s1) (== 0 a) (== 0 b) (== 0 c) (== in d)]))))

[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(0 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(0 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(1 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(1 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(0 0) a b c d) (== `(,a ,b ,c ,d) q))) '((1 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(0 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 1 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(1 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 1 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(1 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 1))]

(define dmux8Wayo ; demultiplexor
  (lambda (in sel a b c d e f g h)
    (fresh (s0 s1 s2 sd)
      (== `(,s0 ,s1 ,s2 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== 0 s2) (== in a) (== 0 b) (== 0 c) (== 0 d) (== 0 e) (== 0 f) (== 0 g) (== 0 h)]
        [(== 0 s0) (== 0 s1) (== 1 s2) (== 0 a) (== in b) (== 0 c) (== 0 d) (== 0 e) (== 0 f) (== 0 g) (== 0 h)]
        [(== 0 s0) (== 1 s1) (== 0 s2) (== 0 a) (== 0 b) (== in c) (== 0 d) (== 0 e) (== 0 f) (== 0 g) (== 0 h)]
        [(== 0 s0) (== 1 s1) (== 1 s2) (== 0 a) (== 0 b) (== 0 c) (== in d) (== 0 e) (== 0 f) (== 0 g) (== 0 h)]
        [(== 1 s0) (== 0 s1) (== 0 s2) (== 0 a) (== 0 b) (== 0 c) (== 0 d) (== in e) (== 0 f) (== 0 g) (== 0 h)]
        [(== 1 s0) (== 0 s1) (== 1 s2) (== 0 a) (== 0 b) (== 0 c) (== 0 d) (== 0 e) (== in f) (== 0 g) (== 0 h)]
        [(== 1 s0) (== 1 s1) (== 0 s2) (== 0 a) (== 0 b) (== 0 c) (== 0 d) (== 0 e) (== 0 f) (== in g) (== 0 h)]
        [(== 1 s0) (== 1 s1) (== 1 s2) (== 0 a) (== 0 b) (== 0 c) (== 0 d) (== 0 e) (== 0 f) (== 0 g) (== in h)]))))

[check-equal?
  (run* (q)
    (fresh (a b c d e f g h)
      (dmux8Wayo 1 '(1 0 1) a b c d e f g h) (== `(,a ,b ,c ,d ,e ,f ,g ,h) q))) '((0 0 0 0 0 1 0 0))]

[check-equal?
  (run* (q)
    (fresh (a b c d e f g h)
      (dmux8Wayo 1 '(1 1 1) a b c d e f g h) (== `(,a ,b ,c ,d ,e ,f ,g ,h) q))) '((0 0 0 0 0 0 0 1))]

[check-equal?
  (run* (q)
    (fresh (a b c d e f g h)
      (dmux8Wayo 0 '(1 1 1) a b c d e f g h) (== `(,a ,b ,c ,d ,e ,f ,g ,h) q))) '((0 0 0 0 0 0 0 0))]
