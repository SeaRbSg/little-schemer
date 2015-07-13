#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require rackunit)

; (ifa (== x 'Q) (none y) (only_once y)))))
; (define not_at (lambda (square x) (ifa (== square x) u# s#)))
; (define none (lambda (l x) (fresh (a d) (conso a d l) (conda [(not_at a x)] [else (none d x)]))))
; (define (memebero x l) (conde [(caro l x)] [(fresh (d) (cdro l d) (memebero x d))]))

(define (nono l x)
  (conde
    [(nullo l)]
    [(fresh (a d)
       (conso a d l)
       (ifa (== a x) u# (nono d x)))]))

[check-equal?
  (run* (q)
    (fresh (a b c)
      (all
        (conde [(== a 9)] [(== a '*)])
        (conde [(== b 9)] [(== b '*)])
        (conde [(== c 9)] [(== c '*)]))
      (all
        (nono (list a b) 9)
        (nono (list b) 9))
      (== q `(,a ,b ,c))))
  '((* * 9) (* * *))]

(define only_once
  (lambda (l x)
    (fresh (a d)
      (conso a d l)
      (conda
        [(== a x) (nono d x)]
        [(only_once d x)]))))

[check-equal?
  (run* (q)
    (fresh (a b c d)
      (== q (list a b c d))
      (all
        (conde [(== a 9)] [(== a '*)])
        (conde [(== b 9)] [(== b '*)])
        (conde [(== c 9)] [(== c '*)])
        (conde [(== d 9)] [(== d '*)]))
      (only_once q 9)))
  '((9 * * *) (* 9 * *) (* * 9 *) (* * * 9))]

[check-equal?
  (run* (q)
    (fresh (a b c d h v)
      (== q (list a b c d))
      (== h (list a b c))
      (== v (list c d))
      (all
        (conde [(== a 8)] [(== a '*)])
        (conde [(== b 8)] [(== b '*)])
        (conde [(== c 8)] [(== c '*)])
        (conde [(== d 8)] [(== d '*)]))
      (only_once q 8)
      (only_once h 8)
      (only_once v 8)))
  '((* * 8 *))]

(define at_most_once
  (lambda (l x)
    (fresh (a d)
      (conso a d l)
      (conda
        [(nullo d) s#]
        [(== a x) (nono d x)]
        [else (at_most_once d x)]))))

[check-equal?
  (run* (q)
    (fresh (a b c d h v)
      (== q (list a b c d))
      (== h (list a b c))
      (== v (list c d))
      (all
        (conde [(== a 8)] [(== a '*)])
        (conde [(== b 8)] [(== b '*)])
        (conde [(== c 8)] [(== c '*)])
        (conde [(== d 8)] [(== d '*)]))
      (at_most_once q 8)
      (at_most_once h 8)
      (at_most_once v 8)))
  '((8 * * *) (* 8 * *) (* * 8 *) (* * * 8) (* * * *))]

(define at_most_once_each
  (lambda (listas thingy)
    (conde
      [(nullo listas)]
      [(fresh (a d)
         (conso a d listas)
         (at_most_once a thingy)
         (at_most_once_each d thingy))])))

(define initialize
  (lambda (board thingy)
    (conde
      [(nullo board)]
      [(fresh (a d)
         (conso a d board)
         (conde [(== a thingy)] [(== a '_)])
         (initialize d thingy))])))

[check-equal?
  (run* (board)
    (fresh (Q a1 a2 b1 b2)
      (== Q 'Q)
      (== board (list a1 a2 b1 b2))
      (initialize board Q)
      (fresh (h1 h2 v1 v2 d1 d2)
        (== h1 (list a1 a2)) (== h2 (list b1 b2))
        (== v1 (list a1 b1)) (== v2 (list a2 b2))
        (== d1 (list a1 b2)) (== d2 (list a2 b1))
        (at_most_once_each (list h1 h2 v1 v2 d1 d2) Q)
        )))
  '((Q _ _ _) (_ Q _ _) (_ _ Q _) (_ _ _ Q) (_ _ _ _))]

(run* (board)
  (fresh (Q a1 a2 a3 b1 b2 b3 c1 c2 c3)
    (== Q 'Q)
    (== board (list a1 a2 a3 b1 b2 b3 c1 c2 c3))
    (initialize board Q)
    (fresh (h1 h2 h3 v1 v2 v3 d1 d2 d3 d4 d5 d6)
      (== h1 (list a1 a2 a3)) (== h2 (list b1 b2 b3)) (== h3 (list c1 c2 c3))
      (== v1 (list a1 b1 c1)) (== v2 (list a2 b2 c2)) (== v3 (list a3 b3 c3))
      (== d1 (list a1 b2 c3)) (== d2 (list a3 b2 c1))
      (at_most_once_each (list h1 h2 h3 v1 v2 v3 d1 d2) Q)
      )))

