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

(run 1 (board)
  (fresh (Q a1 a2 a3 a4 a5 a6 a7 a8
            b1 b2 b3 b4 b5 b6 b7 b8
            c1 c2 c3 c4 c5 c6 c7 c8
            d1 d2 d3 d4 d5 d6 d7 d8
            e1 e2 e3 e4 e5 e6 e7 e8
            f1 f2 f3 f4 f5 f6 f7 f8
            g1 g2 g3 g4 g5 g6 g7 g8
            h1 h2 h3 h4 h5 h6 h7 h8)
    (== Q 'Q)
    (== board (list a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8
                    c1 c2 c3 c4 c5 c6 c7 c8 d1 d2 d3 d4 d5 d6 d7 d8
                    e1 e2 e3 e4 e5 e6 e7 e8 f1 f2 f3 f4 f5 f6 f7 f8
                    g1 g2 g3 g4 g5 g6 g7 g8 h1 h2 h3 h4 h5 h6 h7 h8))
    (initialize board Q)
    (fresh (→1 →2 →3 →4 →5 →6 →7 →8
            ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ↓7 ↓8
            ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇗10 ⇗11 ⇗12 ⇗13
            ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9 ⇘10 ⇘11 ⇘12 ⇘13)

      (== →1 (list a1 a2 a3 a4 a5 a6 a7 a8))
      (== →2 (list b1 b2 b3 b4 b5 b6 b7 b8))
      (== →3 (list c1 c2 c3 c4 c5 c6 c7 c8))
      (== →4 (list d1 d2 d3 d4 d5 d6 d7 d8))
      (== →5 (list e1 e2 e3 e4 e5 e6 e7 e8))
      (== →6 (list f1 f2 f3 f4 f5 f6 f7 f8))
      (== →7 (list g1 g2 g3 g4 g5 g6 g7 g8))
      (== →8 (list h1 h2 h3 h4 h5 h6 h7 h8))

      (== ↓1 (list a1 b1 c1 d1 e1 f1 g1 h1))
      (== ↓2 (list a2 b2 c2 d2 e2 f2 g2 h2))
      (== ↓3 (list a3 b3 c3 d3 e3 f3 g3 h3))
      (== ↓4 (list a4 b4 c4 d4 e4 f4 g4 h4))
      (== ↓5 (list a5 b5 c5 d5 e5 f5 g5 h5))
      (== ↓6 (list a6 b6 c6 d6 e6 f6 g6 h6))
      (== ↓7 (list a7 b7 c7 d7 e7 f7 g7 h7))
      (== ↓8 (list a8 b8 c8 d8 e8 f8 g8 h8))

      (== ⇗1  (list b1 a2))
      (== ⇗2  (list c1 b2 a3))
      (== ⇗3  (list d1 c2 b3 a4))
      (== ⇗4  (list e1 d2 c3 b4 a5))
      (== ⇗5  (list f1 e2 d3 c4 b5 a6))
      (== ⇗6  (list g1 f2 e3 d4 c5 b6 a7))
      (== ⇗7  (list h1 g2 f3 e4 d5 c6 b7 a8))
      (== ⇗8  (list h2 g3 f4 e5 d6 c7 b8))
      (== ⇗9  (list h3 g4 f5 e6 d7 c8))
      (== ⇗10 (list h4 g5 f6 e7 d8))
      (== ⇗11 (list h5 g6 f7 e8))
      (== ⇗12 (list h6 g7 f8))
      (== ⇗13 (list h7 g8))

      (== ⇘1  (list a7 b8))
      (== ⇘2  (list a6 b7 c8))
      (== ⇘3  (list a5 b6 c7 d8))
      (== ⇘4  (list a4 b5 c6 d7 e8))
      (== ⇘5  (list a3 b4 c5 d6 e7 f8))
      (== ⇘6  (list a2 b3 c4 d5 e6 f7 g8))
      (== ⇘7  (list a1 b2 c3 d4 e5 f6 g7 h8))
      (== ⇘8  (list b1 c2 d3 e4 f5 g6 h7))
      (== ⇘9  (list c1 d2 e3 f4 g5 h6))
      (== ⇘10 (list d1 e2 f3 g4 h5))
      (== ⇘11 (list e1 f2 g3 h4))
      (== ⇘12 (list f1 g2 h3))
      (== ⇘13 (list g1 h2))

      (at_most_once_each (list →1 →2 →3 →4 →5 →6 →7 →8) Q)
      (at_most_once_each (list ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ↓7 ↓8) Q)
      (at_most_once_each (list ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇗10 ⇗11 ⇗12 ⇗13) Q)
      (at_most_once_each (list ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9 ⇘10 ⇘11 ⇘12 ⇘13) Q)
      )))

