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

(define no_adj
  (lambda (guy1 guy2 floors)
    (fresh (x1 x2 y)
      (conso x1 y floors)
      (caro y x2)
      (conda
        [(== x1 guy1) (not_at guy2 x2)]
        [(== x1 guy2) (not_at guy1 x2)]
        [(no_adj guy1 guy2 y)]))))

; (define above
;   (lambda (top bottom floors)
;     (fresh (x1 x2 y)
;       (conso x1 y floors)
;       (caro y x2)
;       (conde
;         [(== x2 top) (not_at bottom x1)]
;         [(above top bottom y)]))))

(run* (brix)
  (fresh (a b c d e)
    (== brix (list a b c d e))

    (residents brix '(adam bill dale cora erin))

    (not_at 'adam a)                     ; Adam is not at top floor
    (not_at 'bill e)                     ; Bill is not at ground floor
    (not_ea 'cora (list a e))            ; Cora is neither at top nor ground

    ; (above 'dale 'bill brix)             ; Dale lives above Bill

    (no_adj 'erin 'cora brix)            ; Erin and Cora don't live in adj floors
    (no_adj 'cora 'bill brix)            ; Cora and Bill don't live in adj floors
    ))

