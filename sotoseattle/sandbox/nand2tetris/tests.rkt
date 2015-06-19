#lang racket
(require "../../basic_defs.rkt")
(require "../../lib/shared.rkt")
(require "../../../lib/mk.rkt")

(require "./gates.rkt")
(require "./alu.rkt")

(require rackunit)
(require racket/trace)

; Week1 :: gates.rkt :: Basic Gates Tests

[check-equal?  (run* (q) (noto 1 q)) '(0)]
[check-equal?  (run* (q) (noto 0 q)) '(1)]

[check-equal?  (run* (q) (ando 0 0 q)) '(0)]
[check-equal?  (run* (q) (ando 0 1 q)) '(0)]
[check-equal?  (run* (q) (ando 1 0 q)) '(0)]
[check-equal?  (run* (q) (ando 1 1 q)) '(1)]

[check-equal? (run* (q) (oro 0 0 q)) '(0)]
[check-equal? (run* (q) (oro 0 1 q)) '(1)]
[check-equal? (run* (q) (oro 1 0 q)) '(1)]
[check-equal? (run* (q) (oro 1 1 q)) '(1)]

[check-equal? (run* (q) (xoro 0 0 q)) '(0)]
[check-equal? (run* (q) (xoro 0 1 q)) '(1)]
[check-equal? (run* (q) (xoro 1 0 q)) '(1)]
[check-equal? (run* (q) (xoro 1 1 q)) '(0)]

[check-equal? (run* (q) (muxo 0 0 0 q)) '(0)]
[check-equal? (run* (q) (muxo 0 1 0 q)) '(0)]
[check-equal? (run* (q) (muxo 1 0 0 q)) '(1)]
[check-equal? (run* (q) (muxo 1 1 0 q)) '(1)]
[check-equal? (run* (q) (muxo 0 0 1 q)) '(0)]
[check-equal? (run* (q) (muxo 0 1 1 q)) '(1)]
[check-equal? (run* (q) (muxo 1 0 1 q)) '(0)]
[check-equal? (run* (q) (muxo 1 1 1 q)) '(1)]

[check-equal? (run* (q) (fresh (a b) (dmuxo 0 0 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 0 1 a b) (== `(,a ,b) q))) '((0 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 1 0 a b) (== `(,a ,b) q))) '((1 0))]
[check-equal? (run* (q) (fresh (a b) (dmuxo 1 1 a b) (== `(,a ,b) q))) '((0 1))]

[check-equal? (run* (q) (not16o '(1 1 0 1 0) q)) '((0 0 1 0 1))]
[check-equal? (run* (q) (not16o '(0 0 0 1 0) q)) '((1 1 1 0 1))]
[check-equal? (run* (q) (not16o '(1) q)) '((0))]
[check-equal? (run* (q) (not16o '(0 0) q)) '((1 1))]

[check-equal?  (run* (q) (and16o '(0 0 0 1) '(1 1 1 1) q)) '((0 0 0 1))]
[check-equal?  (run* (q) (and16o '(1 1 1 1) '(1 1 1 1) q)) '((1 1 1 1))]
[check-equal?  (run* (q) (and16o '(0 0 0 0) '(1 1 1 1) q)) '((0 0 0 0))]
[check-equal?  (run* (q) (and16o '(0 0 0) '(1 1 1 1) q)) '()]
[check-equal?  (run* (q) (and16o '() '(1 1 1 1) q)) '()]

[check-equal?  (run* (q) (or16o '(0 0 0 0 0) '(0 0 0 0 0) q)) '((0 0 0 0 0))]
[check-equal?  (run* (q) (or16o '(0 0 0 0 0) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (or16o '(1 1 1 1 1) '(1 1 1 1 1) q)) '((1 1 1 1 1))]
[check-equal?  (run* (q) (or16o '(1 0 1 0 1) '(0 1 0 1 0) q)) '((1 1 1 1 1))]

[check-equal?  (run* (q) (mux16o '(1 0 1 0 1) '(0 1 0 1 0) 0 q)) '((1 0 1 0 1))]
[check-equal?  (run* (q) (mux16o '(1 0 1 0 1) '(0 1 0 1 0) 1 q)) '((0 1 0 1 0))]

[check-equal? (run* (q) (or8Wayo '(0 0 0 0 0 0 0 0) q)) '(0)]
[check-equal? (run* (q) (or8Wayo '(1 0 1 0 1 0 1 0) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(1 1 1 1 1 1 1 1) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(0 0 0 0 1 0 0 0) q)) '(1)]
[check-equal? (run* (q) (or8Wayo '(0 0 0 0 0 0 0 1) q)) '(1)]

[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(0 0) q)) '((0 0))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(0 1) q)) '((0 1))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(1 0) q)) '((1 0))]
[check-equal?  (run* (q) (mux4Way16o '(0 0) '(0 1) '(1 0) '(1 1) '(1 1) q)) '((1 1))]

[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 0 0) q)) '(a)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 0 1) q)) '(b)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 1 0) q)) '(c)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(0 1 1) q)) '(d)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 0 0) q)) '(e)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 0 1) q)) '(f)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 1 0) q)) '(g)]
[check-equal?  (run* (q) (mux8Way16o 'a 'b 'c 'd 'e 'f 'g 'h '(1 1 1) q)) '(h)]

[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(0 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(0 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(1 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 0 '(1 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(0 0) a b c d) (== `(,a ,b ,c ,d) q))) '((1 0 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(0 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 1 0 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(1 0) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 1 0))]
[check-equal? (run* (q) (fresh (a b c d) (dmux4Wayo 1 '(1 1) a b c d) (== `(,a ,b ,c ,d) q))) '((0 0 0 1))]

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

; Week2 :: alu.rkt :: ALU and adderos tests

[check-equal? (run* (q) (fresh (c r) (halfaddero 0 0 r c) (== `(,r ,c) q))) '((0 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 0 1 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 1 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (halfaddero 1 1 r c) (== `(,r ,c) q))) '((0 1))]

[check-equal? (run* (q) (fresh (c r) (fulladdero 0 0 0 r c) (== `(,r ,c) q))) '((0 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 0 1 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 1 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 0 1 1 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 0 0 r c) (== `(,r ,c) q))) '((1 0))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 0 1 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 1 0 r c) (== `(,r ,c) q))) '((0 1))]
[check-equal? (run* (q) (fresh (c r) (fulladdero 1 1 1 r c) (== `(,r ,c) q))) '((1 1))]

[check-equal? (run* (q) (add16o '(0) '(0) q)) '((0))]
[check-equal? (run* (q) (add16o '(1) '(0) q)) '((1))]
[check-equal? (run* (q) (add16o '(1 1 1 0) '(1 1 0 1) q)) '((1 0 1 1))]
[check-equal? (run* (q) (add16o '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0) '(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (add16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (add16o '(0 0 1 1 1 1 0 0 1 1 0 0 0 0 1 1) '(0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0) q))
              '((0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1))]
[check-equal? (run* (q) (add16o '(0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0) '(1 0 0 1 1 0 0 0 0 1 1 1 0 1 1 0) q))
              '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))]
[check-equal? (run* (q) (add16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))]

[check-equal? (run* (q) (flipo '(1) q)) '((0))]
[check-equal? (run* (q) (flipo '(1 1 1 1) q)) '((0 0 0 0))]
[check-equal? (run* (q) (flipo '(0 0 0 0) q)) '((1 1 1 1))]
[check-equal? (run* (q) (flipo '(1 0 0 1) q)) '((0 1 1 0))]
[check-equal? (run* (q) (flipo '(0 0 1 0 1) q)) '((1 1 0 1 0))]

[check-equal? (run* (q) (inc16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) q)) '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))]
[check-equal? (run* (q) (inc16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q)) '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))]
[check-equal? (run* (q) (inc16o '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1) q)) '((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0))]
[check-equal? (run* (q) (inc16o '(1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1) q)) '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0))]

(define falsy16 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define truzy16 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define test1   '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1))
(define test2   '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
(define testAluo
  (lambda (x y zx nx zy ny f no result)
    (fresh (out zr ng)
      (aluo x y zx nx zy ny f no out ng zr)
      (== (list out zr ng) result))))

;                                 |  x    |   y  |zx|nx|zy|ny| f|no|       |            out               |zr|ng|
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  0  1  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  0  1  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  1  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  1  1  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  0  0  0  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  1  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  0  0  0  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  0  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  1  1  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 1  1  0  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  0  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  0  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  0  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo falsy16  truzy16 0  1  0  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   1  0  1  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 0))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  1  0  1  0 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  1  1  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  1  1  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  0  0  0  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  1  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  0  0  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   0  1  1  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  0  1  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  1  1  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   1  1  0  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  0  0  1  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  1  0  0  1  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  0  1  1  1 q)) '(((1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0) 0 1))]
[check-equal? (run* (q) (testAluo test1    test2   0  0  0  0  0  0 q)) '(((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 0 0))]
[check-equal? (run* (q) (testAluo test1    test2   0  1  0  1  0  1 q)) '(((0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1) 0 0))]
