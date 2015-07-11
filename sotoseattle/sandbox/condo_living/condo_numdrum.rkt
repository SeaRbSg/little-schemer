#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")

; 1. Adam does not live on the top floor.
; 2. Bill does not live on the bottom floor.
; 3. Cora does not live on either the top or the bottom floor.
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

(define living
  (lambda (names floors)
    (conde
      [(nullo names)]
      [(fresh (x y)
         (conso x y names)
         (tenant x floors)
         (living y floors))])))

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

(define higher
  (lambda (top bottom floors)
    (fresh (x1 x2 y)
      (conso x1 y floors)
      (caro y x2)
      (conde
        [(== x1 bottom) (not_ea top y)]
        [(higher top bottom y)]))))

(run* (brix)
  (fresh (PH LV3 LV2 LV1 GROUND)
    (== brix (list PH LV3 LV2 LV1 GROUND))     ; list of floors
    (living '(adam bill dale cora erin) brix)  ; list of tenants
    (not_at 'adam PH)                          ; Adam is not at penthouse
    (not_at 'bill GROUND)                      ; Bill is not at ground floor
    (not_ea 'cora (list PH GROUND))            ; Cora is neither top nor bottom
    (higher 'dale 'bill brix)                  ; Dale lives above Bill
    (no_adj 'erin 'cora brix)                  ; Erin and Cora not in adj floors
    (no_adj 'cora 'bill brix)))                ; Cora and Bill not in adj floors
