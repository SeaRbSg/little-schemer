#lang cKanren

(require cKanren/miniKanren)
(require cKanren/neq)
(require "creasoned.rkt")
(require (prefix-in t: rackunit))

;; n-queens problem
;; Given an n-sized chessboard, place all queens on the board that
;; no queens can attack each other.

(define (memberso m l)
  (conde
    [(nullo m) s#]
    [(fresh (ma md)
       (caro m ma)
       (membero ma l)
       (cdro m md)
       (memberso md l))]))

(define (nums l)
  (map build-num l))

(define (num-list l)
  (map nums l))

(t:check-equal? (nums '(1 2 3 4))
                '((1) (0 1) (1 1) (0 0 1)))

(define (not-diago a b)
  (fresh (d1 d2)
    (conda
      [(minuso a b d1)
       (=/= d1 (build-num 1))]
     [(minuso b a d2)
      (=/= d2 (build-num 1))])))

(check-run* (r)
            (not-diago (build-num 1) r)
            => '(#t))

(define (safeo l)
  (fresh (a d r l)
    (conso a d l)
    (caro d r)
    (not-diago a d)
    (safeo d)))

(define (1-queens out)
  (fresh (q1)
    (== out (list q1))
    (memberso (nums '(1)) out)))

(define (4-queens out)
  (fresh (q1 q2 q3 q4)
    (== out (list q1 q2 q3 q4))
    (memberso (nums '(1 2 3 4)) out)))

(check-run* (r)
            (1-queens r)
            => (num-list '((1))))

(check-run* (r)
            (4-queens r)
            => (num-list '((2 4 1 3)
                           (3 1 4 2))))
