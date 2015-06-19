#lang racket
(require "../../basic_defs.rkt")
(require "../../lib/shared.rkt")
(require "../../../lib/mk.rkt")
(require "./week1.rkt")
(require rackunit)
(require racket/trace)

(define halfaddero
  (lambda (x y sum carry)
    (all
      (xoro x y sum)
      (ando x y carry))))

[check-equal? (run* (q) (fresh (c r) (halfaddero 0 0 r c) (== `(,r ,c) q))) '((0 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 0 1 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 1 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 1 1 r c) (== `(,r ,c) q))) '((0 1))]

(define fulladdero
  (lambda (x y in_carry sum out_carry)
    (fresh (s1 c1 c2)
      (halfaddero x y s1 c1)
      (halfaddero s1 in_carry sum c2)
      (xoro c1 c2 out_carry))))

[check-equal? (run* (q) (fresh (c r) (fulladdero 0 0 0 r c) (== `(,r ,c) q))) '((0 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 0 1 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 1 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 1 1 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 0 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 0 1 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 1 0 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 1 1 r c) (== `(,r ,c) q))) '((1 1))]

(define reverso ; given an n bit list and '() => reverse the list
  (lambda (l acc out)
    (conde
      [(nullo l) (== acc out)]
      [(fresh (a d res)
         (conso a d l)
         (conso a acc res)
         (reverso d res out))])))

(define sub-addero
  (lambda (in_carr a b sum)
    (conde
      [(nullo a) (nullo b) (nullo sum)]
      [(fresh (ac ad bc bd sc sd out_carr)
         (conso ac ad a)
         (conso bc bd b)
         (conso sc sd sum)
         (alli
           (fulladdero ac bc in_carr sc out_carr)
           (sub-addero out_carr ad bd sd)))])))

(define add16o ; the general addero
  (lambda (a b sum)
    (fresh (a_rev b_rev s_rev)
      (reverso a '() a_rev )
      (reverso b '() b_rev)
      (sub-addero 0 a_rev b_rev s_rev)
      (reverso s_rev '() sum))))

[check-equal? (run* (q) (add16o '(0) '(0) q)) '((0))]
[check-equal? (run* (q) (add16o '(1) '(0) q)) '((1))]
[check-equal? (run* (q) (add16o '(1 1 1 0) '(1 1 0 1) q)) '((1 0 1 1))]
[check-equal? (run* (q) (add16o '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
                                '(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (add16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (add16o '(0 0 1 1 1 1 0 0 1 1 0 0 0 0 1 1)
                                '(0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0) q))
              '((0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1))]
[check-equal? (run* (q) (add16o '(0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0)
                                '(1 0 0 1 1 0 0 0 0 1 1 1 0 1 1 0) q))
              '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))]
[check-equal? (run* (q) (add16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))]

(define flipo ; given an n bit list => make a list with all bits flipped
  (lambda (a out)
    (conde
      [(nullo a) (nullo out)]
      [(fresh (ac ad res)
         (conso ac ad a)
         (conde
           [(== 0 ac) (conso 1 res out)]
           [(== 1 ac) (conso 0 res out)])
         (flipo ad res))])))

[check-equal? (run* (q) (flipo '(1) q)) '((0))]
[check-equal? (run* (q) (flipo '(1 1 1 1) q)) '((0 0 0 0))]
[check-equal? (run* (q) (flipo '(0 0 0 0) q)) '((1 1 1 1))]
[check-equal? (run* (q) (flipo '(1 0 0 1) q)) '((0 1 1 0))]
[check-equal? (run* (q) (flipo '(0 0 1 0 1) q)) '((1 1 0 1 0))]

(define oneOfmanyo ; given an n bit list => make a one of same length
  (lambda (a out)
    (fresh (ac ad oc od)
      (conso ac ad a)
      (conde
        [(nullo ad) (== '(1) out)]
        [(conso oc od out)
         (== 0 oc)
         (oneOfmanyo ad od)]))))

[check-equal? (run* (q) (oneOfmanyo '(1 1 1 0 0 1) q)) '((0 0 0 0 0 1))]

(define nego ; given an n bit list => build its negative
  (lambda (a out)
    (fresh (flap one)
      (flipo a flap)
      (oneOfmanyo a one)
      (add16o flap one out))))

[check-equal? (run* (q) (nego '(0 1 0 0) q)) '((1 1 0 0))]

(define inc16o
  (lambda (a out)
    (fresh (one)
      (oneOfmanyo a one)
      (add16o a one out))))

[check-equal? (run* (q) (inc16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) q))
              '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))]
[check-equal? (run* (q) (inc16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))]
[check-equal? (run* (q) (inc16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1) q))
              '((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0))]
[check-equal? (run* (q) (inc16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0))]

(define break16bits ; HARDCODED !!!!!
  (lambda (a p1 p2)
    (fresh (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
      (== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6 ,a7 ,a8 ,a9 ,a10 ,a11 ,a12 ,a13 ,a14 ,a15) a)
      (== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6 ,a7) p1)
      (== `(,a8 ,a9 ,a10 ,a11 ,a12 ,a13 ,a14 ,a15) p2))))

[check-equal?
  (run* (q)
    (fresh (a b)
      (break16bits '(0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1) a b)
      (== `(,a ,b) q)))
  '(((0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1)))]

(define falsy (lambda () '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(define truzy (lambda () '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define aluo ; Will work only for 16 bit numbers
  (lambda (x y zx nx zy ny f no out)
    (fresh (res zr ng x1 !x1 x2 y1 !y1 y2 x+y x&y xy !xy nonzero or1 or2 part1 part2)
      (mux16o x (falsy) zx x1)
      (mux16o y (falsy) zy y1)
      (not16o x1 !x1)
      (mux16o x1 !x1 nx x2)
      (not16o y1 !y1)
      (mux16o y1 !y1 ny y2)
      (and16o x2 y2 x&y)
      (add16o x2 y2 x+y)
      (mux16o x&y x+y f xy)
      (not16o xy !xy)
      (mux16o xy !xy no res)
      (break16bits res part1 part2)
      (or8Wayo part1 or1)
      (or8Wayo part2 or2)
      (oro or1 or2 nonzero)
      (noto nonzero zr)
      (caro res ng)
      (== (list res zr ng) out))))

;                             |  x   |   y  |zx|nx|zy|ny| f|no|       |            out               |zr|ng|
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  0  1  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  1  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  1  1  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  0  0  0  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  1  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  0  0  0  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  0  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  1  1  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 1  1  0  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  0  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  0  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  0  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (falsy) (truzy) 0  1  0  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]

(define test1 (lambda () '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1)))
(define test2 (lambda () '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))

                            ; |  x   |   y  |zx|nx|zy|ny| f|no|       |            out               |zr|ng|
[check-equal? (run* (q) (aluo (test1) (test2) 1  0  1  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  1  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  1  1  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  1  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  0  0  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  1  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  0  0  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  0  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  1  1  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 1  1  0  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  0  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  0  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0) 0 1))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  0  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (aluo (test1) (test2) 0  1  0  1  0  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1) 0 0))]
