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

(define ando
  (lambda (a b out)
    (fresh (x)
      (nando a b x)
      (noto x out))))

(define oro
  (lambda (a b out)
    (fresh (x y)
      (noto a x)
      (noto b y)
      (nando x y out))))

; Basic logic gates based on above

(define xoro
  (lambda (a b out)
    (fresh (x y w z)
      (noto a x)
      (noto b y)
      (ando a y w)
      (ando b x z)
      (oro w z out))))

(define muxo ; multiplexor
  (lambda (a b sel out)
    (conde
      [(== 0 sel) (== a out)]
      [(== 1 sel) (== b out)])))

(define dmuxo ; demultiplexor
  (lambda (in sel a b)
    (conde
      [(== 0 sel) (== in a) (== 0  b)]
      [(== 1 sel) (== 0  a) (== in b)])))

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

(define mux16o
  (lambda (a b sel out)
      (conde
        [(== 0 sel) (== a out)]
        [(== 1 sel) (== b out)])))

(define or8Wayo
  (lambda (a out)
      (conde
        [(nullo a) (== 0 out)]
        [(fresh (ac ad)
           (conso ac ad a)
           (conde
             [(== 1 ac) (== 1 out)]
             [(== 0 ac) (or8Wayo ad out)]))])))

(define mux4Way16o ; 4 way multiplexor
  (lambda (a b c d sel out)
    (fresh (s0 s1 sd)
      (== `(,s0 ,s1 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== a out)]
        [(== 0 s0) (== 1 s1) (== b out)]
        [(== 1 s0) (== 0 s1) (== c out)]
        [(== 1 s0) (== 1 s1) (== d out)]))))

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

(define dmux4Wayo ; demultiplexor
  (lambda (in sel a b c d)
    (fresh (s0 s1 sd)
      (== `(,s0 ,s1 . ,sd) sel)
      (conde
        [(== 0 s0) (== 0 s1) (== in a) (== 0 b) (== 0 c) (== 0 d)]
        [(== 0 s0) (== 1 s1) (== 0 a) (== in b) (== 0 c) (== 0 d)]
        [(== 1 s0) (== 0 s1) (== 0 a) (== 0 b) (== in c) (== 0 d)]
        [(== 1 s0) (== 1 s1) (== 0 a) (== 0 b) (== 0 c) (== in d)]))))

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
