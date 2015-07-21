#lang cKanren

(require cKanren/miniKanren)
(require cKanren/neq)
(require "creasoned.rkt")

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

(define (highero a b l)
  (conde
    [(caro l b)
     (membero a l)]
    [(fresh (d)
       (cdro l d)
       (highero a b d))]))

(check-run* (a)
            (highero 'adam 'bill '(bill bob adam))
            (== #t a)
            => '(#t))

(check-run* (a)
            (highero 'adam 'bob '(bill adam bob))
            (== #t a)
            => '())

(define (eq-caro l x)
  (caro l x))

(define (neq-caro l x)
  (fresh (a)
    (caro l a)
    (=/= a x)))

(check-run* (q)
            (eq-caro '(bill bob) 'bill)
            (== q #t)
            => '(#t))

(check-run* (q)
            (neq-caro '(bill bob) 'bob)
            (== q #t)
            => '(#t))

(define (not-nexto x1 x2 l)
  (fresh (a d)
    (conso a d l)
    (conda
      [(== a x1) (neq-caro d x2)]
      [(== a x2) (neq-caro d x1)]
      [(not-nexto x1 x2 d)])))

(check-run* (a)
            (not-nexto 'bill 'bob '(bill bob adam))
            (== #t a)
            => '())

(check-run* (a)
            (not-nexto 'bill 'bob '(bill adam bob))
            (== #t a)
            => '(#t))

; 1. Adam does not live on the top floor.
; 2. Bill does not live on the bottom floor.
; 3. Cora does not live on either the top or the bottom floor.
; 4. Dale lives on a higher floor than does Bill.
; 5. Erin does not live on a floor adjacent to Cora's.
; 6. Cora does not live on a floor adjacent to Bill's.

(define (aparmento floors)
  (fresh (f1 f2 f3 f4 f5)
    (== floors (list f1 f2 f3 f4 f5))
    (memberso '(adam bill cora dale erin) floors)
    (=/= 'adam f5)
    (=/= 'bill f1)
    (=/= 'cora f1) (=/= 'cora f5)
    (highero 'dale 'bill floors)
    (not-nexto 'erin 'cora floors)
    (not-nexto 'cora 'bill floors)))

(check-run* (floors)
  (aparmento floors)
  => '((erin bill adam cora dale)))
