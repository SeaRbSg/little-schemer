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

(define reverso
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

(define addero
  (lambda (a b sum)
    (fresh (a_rev b_rev s_rev)
      (reverso a '() a_rev )
      (reverso b '() b_rev)
      (sub-addero 0 a_rev b_rev s_rev)
      (reverso s_rev '() sum))))

[check-equal? (run* (q) (addero '(0) '(0) q)) '((0))]
[check-equal? (run* (q) (addero '(1) '(0) q)) '((1))]
[check-equal? (run* (q) (addero '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
                                '(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (addero '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[check-equal? (run* (q) (addero '(0 0 1 1 1 1 0 0 1 1 0 0 0 0 1 1)
                                '(0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0) q))
              '((0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1))]
[check-equal? (run* (q) (addero '(0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0)
                                '(1 0 0 1 1 0 0 0 0 1 1 1 0 1 1 0) q))
              '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))]
[check-equal? (run* (q) (addero '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) q))
              '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))]

