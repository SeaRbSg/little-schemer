#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")
(require "ch23.rkt")

;; 1. Adam does not live on the top floor.
;; 2. Bill does not live on the bottom floor.
;; 3. Cora does not live on either the top or the bottom floor.
;; 4. Dale lives on a higher floor than does Bill.
;; 5. Erin does not live on a floor adjacent to Cora's.
;; 6. Cora does not live on a floor adjacent to Bill's.

(define (higher-than hi lo l)
  (fresh (a d)
    (cons° a d l)
    (cond-e [(≈ lo a) (member° hi d)]
            [(higher-than hi lo d)])))

(define (non-adjacent p1 p2 l)
  (cond-e [(null° l)]
          [(fresh (a d)
             (cons° a d l)
             (cond-a [(all (≈ a p1) (eq-car° d p2)) %u]
                     [(all (≈ a p2) (eq-car° d p1)) %u]
                     [else (non-adjacent p1 p2 d)]))]))

(define (members° l1 l2)
  (cond-e [(null° l1)]
          [(fresh (a d)
             (cons° a d l1)
             (member° a l2)
             (members° d l2))]))

(define (apartments floors)
  (fresh (f1 f2 f3 f4 f5)

    ;; 0. Basic housekeeping
    (≈ floors (list f1 f2 f3 f4 f5))
    (members° '(adam bill cora dale erin) floors)

    ;; 1. Adam does not live on the top floor.
    (member° 'adam (list f1 f2 f3 f4   )) ; (not-on 'adam f5)

    ;; 2. Bill does not live on the bottom floor.
    (member° 'bill (list    f2 f3 f4 f5)) ; (not-on 'bill f1)

    ;; 3. Cora does not live on either the top or the bottom floor.
    (member° 'cora (list    f2 f3 f4   )) ; (not-on 'cora f1) (not-on 'cora f5)

    ;; 4. Dale lives on a higher floor than does Bill.
    (higher-than 'dale 'bill floors)

    ;; 5. Erin does not live on a floor adjacent to Cora's.
    (non-adjacent 'erin 'cora floors)

    ;; 6. Cora does not live on a floor adjacent to Bill's.
    (non-adjacent 'cora 'bill floors)))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (q)
              (non-adjacent 'a 'b '())
              (≈ q #t)
              => '(#t))

  (check-run* (q)
              (non-adjacent 'a 'b '(a c b))
              (≈ q #t)
              => '(#t))

  (check-run* (q)
              (non-adjacent 'a 'b '(b c a))
              (≈ q #t)
              => '(#t))

  (check-run* (q)
              (non-adjacent 'a 'b '(a b c))
              (≈ q 'wtf)
              => none)

  (check-run* (q)
              (non-adjacent 'a 'b '(b a c))
              (≈ q 'wtf)
              => none)

  (check-run* (floors)
              (apartments floors)
              => '((erin bill adam cora dale))))
