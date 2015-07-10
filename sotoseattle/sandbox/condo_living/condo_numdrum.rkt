#lang racket
(require "../../basic_defs.rkt")
(require "../../lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../../../lib/mk.rkt")

; 1. Adam does not live on the top floor. A => B C D E
; 2. Bill does not live on the bottom floor. E => A B C D
; 3. Cora does not live on either the top or the bottom floor. => B C D
; 4. Dale lives on a higher floor than does Bill.
; 5. Erin does not live on a floor adjacent to Cora's.
; 6. Cora does not live on a floor adjacent to Bill's.

(define tenant
  (lambda (guy l)
    (fresh (floor d)
      (conso floor d l)
      (conde
        [(== floor guy)]
        [(tenant guy d)]))))

(define residents
  (lambda (floors names)
    (conde
      [(nullo names)]
      [(fresh (x y)
         (conso x y names)
         (tenant x floors)
         (residents floors y))])))

(define not_at
  (lambda (guy floor)
    (ifa (== guy floor) u# s#)))

(define not_ea
  (lambda (guy floors)
      (conda
        [(nullo floors)]
        [(fresh (x y)
           (conso x y floors)
           (alli
             (not_at guy x)
             (not_ea guy y)))])))

(run* (brix)
  (fresh (a b c)
    (== brix (list a b c))

    (residents brix '(adam pepe lola))

    (not_ea 'adam (list a b))
    (not_at 'lola b)
    (not_at 'pepe c)
    ))

