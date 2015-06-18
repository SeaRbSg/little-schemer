#lang racket
(require "../../basic_defs.rkt")
(require "../../lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../../../lib/mk.rkt")

; basic building block NAND

(define nando
  (lambda (a b out)
    (conde
      [(== 0 a) (== 0 b) (== 1 out)]
      [(== 0 a) (== 1 b) (== 1 out)]
      [(== 1 a) (== 0 b) (== 1 out)]
      [(== 1 a) (== 1 b) (== 0 out)])))

; Most basic logic gates derived from nand

(define noo
  (lambda (a out)
    (nando a a out)))

[check-equal?  (run* (q) (noo 1 q)) '(0)]
[check-equal?  (run* (q) (noo 0 q)) '(1)]

(define ano
  (lambda (a b out)
    (fresh (x)
      (nando a b x)
      (noo x out))))

[check-equal?  (run* (q) (ano 0 0 q)) '(0)]
[check-equal?  (run* (q) (ano 0 1 q)) '(0)]
[check-equal?  (run* (q) (ano 1 0 q)) '(0)]
[check-equal?  (run* (q) (ano 1 1 q)) '(1)]

(define oro
  (lambda (a b out)
    (fresh (x y)
      (noo a x)
      (noo b y)
      (nando x y out))))

[check-equal? (run* (q) (oro 0 0 q)) '(0)]
[check-equal? (run* (q) (oro 0 1 q)) '(1)]
[check-equal? (run* (q) (oro 1 0 q)) '(1)]
[check-equal? (run* (q) (oro 1 1 q)) '(1)]

; Basic logic gates based on above

(define xoro
  (lambda (a b out)
    (fresh (x y w z)
      (noo a x)
      (noo b y)
      (ano a y w)
      (ano b x z)
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

; (define ano_muxo_oro
;   (lambda (a b sel out)
;     (conde
;       [(== 0 sel) (ano a b out)]
;       [(== 1 sel) (oro a b out)])))
;
; [check-equal? (run* (q) (ano_muxo_oro 0 0 0 q)) '(0)]
; [check-equal? (run* (q) (ano_muxo_oro 0 1 0 q)) '(0)]
; [check-equal? (run* (q) (ano_muxo_oro 1 0 0 q)) '(0)]
; [check-equal? (run* (q) (ano_muxo_oro 1 1 0 q)) '(1)]
; [check-equal? (run* (q) (ano_muxo_oro 0 0 1 q)) '(0)]
; [check-equal? (run* (q) (ano_muxo_oro 0 1 1 q)) '(1)]
; [check-equal? (run* (q) (ano_muxo_oro 1 0 1 q)) '(1)]
; [check-equal? (run* (q) (ano_muxo_oro 1 1 1 q)) '(1)]

(define demuxo ; demultiplexor
  (lambda (in sel a b)
    (conde
      [(== 0 sel) (== in a) (== 0  b)]
      [(== 1 sel) (== 0  a) (== in b)])))

[check-equal? (run* (q) (fresh (a b) (demuxo 0 0 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (demuxo 0 1 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (demuxo 1 0 a b) (== `(,a ,b) q))) '((1 0))]
[check-equal? (run* (q) (fresh (a b) (demuxo 1 1 a b) (== `(,a ,b) q))) '((0 1))]

; n variants like 16bit variants

(define nono
  (lambda (a out)
    (conde
      [(nullo a) (nullo out)]
      [(fresh (ac ad oc od)
         (conso ac ad a)
         (conso oc od out)
         (noo ac oc)
         (nono ad od))])))

[check-equal? (run* (q) (nono '(1 1 0 1 0) q)) '((0 0 1 0 1))]
[check-equal? (run* (q) (nono '(0 0 0 1 0) q)) '((1 1 1 0 1))]
[check-equal? (run* (q) (nono '(1) q)) '((0))]
[check-equal? (run* (q) (nono '(0 0) q)) '((1 1))]

(define anono ; and for n buses
  (lambda (a b out)
    (conde
      [(nullo a) (nullo b) (nullo out)]
      [(fresh (ac ad bc bd oc od)
         (conso ac ad a)
         (conso bc bd b)
         (conso oc od out)
         (ano ac bc oc)
         (anono ad bd od))])))

[check-equal?  (run* (q) (anono '(0 0 0 1) '(1 1 1 1) q)) '((0 0 0 1))]
[check-equal?  (run* (q) (anono '(1 1 1 1) '(1 1 1 1) q)) '((1 1 1 1))]
[check-equal?  (run* (q) (anono '(0 0 0 0) '(1 1 1 1) q)) '((0 0 0 0))]
[check-equal?  (run* (q) (anono '(0 0 0) '(1 1 1 1) q)) '()]
[check-equal?  (run* (q) (anono '() '(1 1 1 1) q)) '()]

(define orono
  (lambda (a b out)
    (conde
      [(nullo a) (nullo b) (nullo out)]
      [(fresh (ac ad bc bd oc od)
         (conso ac ad a)
         (conso bc bd b)
         (conso oc od out)
         (oro ac bc oc)
         (orono ad bd od))])))

[check-equal?  (run* (q) (orono '(0 0 0 0 0) '(0 0 0 0 0) q)) '((0 0 0 0 0))]
[check-equal?  (run* (q) (orono '(0 0 0 0 0) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (orono '(1 1 1 1 1) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (orono '(1 0 1 0 1) '(0 1 0 1 0) q)) '((1 1 1 1 1))]

; ----------------------------------------------------

(define muxo4o ; 4 way multiplexor
  (lambda (a b c d sel out)
    (fresh (s0 s1 sd)
      (conso s0 sd sel)
      (caro sd s1)
      (conde
        [(== 0 s0) (== 0 s1) (== a out)]
        [(== 0 s0) (== 1 s1) (== b out)]
        [(== 1 s0) (== 0 s1) (== c out)]
        [(== 1 s0) (== 1 s1) (== d out)]))))

[check-equal?  (run* (q) (muxo4o '(0 0) '(0 1) '(1 0) '(1 1) '(0 0) q)) '((0 0))]
[check-equal?  (run* (q) (muxo4o '(0 0) '(0 1) '(1 0) '(1 1) '(0 1) q)) '((0 1))]
[check-equal?  (run* (q) (muxo4o '(0 0) '(0 1) '(1 0) '(1 1) '(1 0) q)) '((1 0))]
[check-equal?  (run* (q) (muxo4o '(0 0) '(0 1) '(1 0) '(1 1) '(1 1) q)) '((1 1))]











