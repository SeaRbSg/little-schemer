#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require rackunit)

(define (nono l x)
  (conde
    [(nullo l)]
    [(fresh (a d)
       (conso a d l)
       (ifa (== a x) u# (nono d x)))]))

(define at_most_once
  (lambda (l x)
    (fresh (a d)
      (conso a d l)
      (conda
        [(nullo d) s#]
        [(== a x) (nono d x)]
        [else (at_most_once d x)]))))

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

(run* (board)
  (fresh (Q a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
    (== Q 'Q)
    (== board (list a1 a2 a3 a4
                    b1 b2 b3 b4
                    c1 c2 c3 c4
                    d1 d2 d3 d4))
    (initialize board Q)
    (fresh (→1 →2 →3 →4
               ↓1 ↓2 ↓3 ↓4
               ⇗1 ⇗2 ⇗3 ⇗4 ⇗5
               ⇘1 ⇘2 ⇘3 ⇘4 ⇘5)

      (== →1 (list a1 a2 a3 a4))
      (== →2 (list b1 b2 b3 b4))
      (== →3 (list c1 c2 c3 c4))
      (== →4 (list d1 d2 d3 d4))

      (== ↓1 (list a1 b1 c1 d1))
      (== ↓2 (list a2 b2 c2 d2))
      (== ↓3 (list a3 b3 c3 d3))
      (== ↓4 (list a4 b4 c4 d4))

      (== ⇗1  (list b1 a2))
      (== ⇗2  (list c1 b2 a3))
      (== ⇗3  (list d1 c2 b3 a4))
      (== ⇗4  (list d2 c3 b4))
      (== ⇗5  (list d3 c4))

      (== ⇘1  (list a3 b4))
      (== ⇘2  (list a2 b3 c4))
      (== ⇘3  (list a1 b2 c3 d4))
      (== ⇘4  (list b1 c2 d3))
      (== ⇘5  (list c1 d2))

      (at_most_once_each (list →1 →2 →3 →4
                               ↓1 ↓2 ↓3 ↓4
                               ⇗1 ⇗2 ⇗3 ⇗4 ⇗5
                               ⇘1 ⇘2 ⇘3 ⇘4 ⇘5) Q)
      )))

; includes the solution _ Q _ _  with most queens in various rotations
;                       _ _ _ Q
;                       Q _ _ _
;                       _ _ Q _

