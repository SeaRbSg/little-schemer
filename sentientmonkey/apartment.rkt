#lang racket/base
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch22.rkt")
(require "ch23.rkt")
(require "ch24.rkt")
(require "ch26.rkt")
(require "ch30.rkt")

; 1. Adam does not live on the top floor.
; 2. Bill does not live on the bottom floor.
; 3. Cora does not live on either the top or the bottom floor.
; 4. Dale lives on a higher floor than does Bill.
; 5. Erin does not live on a floor adjacent to Cora's.
; 6. Cora does not live on a floor adjacent to Bill's.

(define (topo x l)
  (caro l x))

(check-run* (r)
            (topo 'adam r)
            => '((adam . _.0)))

(define (bottomo x l)
  (conde
    [(nullo l) u#]
    [(caro l x) (cdro l '())]
    [else
      (fresh (d)
        (cdro l d)
        (bottomo x d))]))

(check-run* (r)
            (bottomo r '(bill bob adam))
            => '(adam))

(define (noto g)
  (conda
    [g u#]
    [else s#]))

(define (nototopo x l)
  (noto (topo x l)))

(check-run* (r)
            (== '(bill bob adam) r)
            (nototopo 'adam r)
            => '((bill bob adam)))

(check-run* (r)
            (== '(adam bill bob) r)
            (nototopo 'adam r)
            => '())

(define (notobottomo x l)
  (noto (bottomo x l)))

(check-run* (r)
            (== '(bill bob adam) r)
            (notobottomo 'adam r)
            => '())

(check-run* (r)
            (== '(adam bill bob) r)
            (notobottomo 'adam r)
            => '((adam bill bob)))

(define (nexto a b l)
  (fresh (x)
    (caro l a)
    (cdro l x)
    (caro x b)))

(check-run* (r)
            (== '(bill bob adam) r)
            (nexto 'bill 'bob r)
            => '((bill bob adam)))

(check-run* (r)
            (== '(bill adam bob) r)
            (nexto 'bill 'bob r)
            => '())

(define (notonexto a b l)
  (noto (nexto a b l)))

(check-run* (a)
            (notonexto 'bill 'bob '(bill bob adam))
            (== #t a)
            => '())

(check-run* (a)
            (notonexto 'bill 'bob '(bill adam bob))
            (== #t a)
            => '(#t))

(define (highero a b l)
  (conde
    [(caro l a)
     (membero b l)]
    [(fresh (d)
       (cdro l d)
       (highero a b d))]))

(check-run* (a)
            (highero 'bill 'adam '(bill bob adam))
            (== #t a)
            => '(#t))

(check-run* (a)
            (highero 'bob 'bill '(bill adam bob))
            (== #t a)
            => '())

(define (memberso m l)
  (conde
    [(nullo m) s#]
    [(fresh (ma md)
       (caro m ma)
       (membero ma l)
       (cdro m md)
       (memberso md l))]))

(check-run* (a)
            (memberso '(bill bob) '(bill bob adam))
            (== #t a)
            => '(#t))

(check-run* (a)
            (memberso '(bill susie bob) '(bill bob adam))
            (== #t a)
            => '())

(define (permuteo x y)
  (fresh (xa xd yd)
    (conso xa xd x)
    (cdro y yd)
    (permuteo xd yd)
    (rembero xa y yd)))

(check-run 1 (a)
            (permuteo '(bill susie bob) a)
            => '(#t))
;; fails

