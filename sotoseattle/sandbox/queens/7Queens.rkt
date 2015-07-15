#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require "4Queens.rkt")
(require "5Queens.rkt")
(require "6Queens.rkt")
(require rackunit)

(provide 7Queens)

(define 7Queens
  (lambda ()
    (run* (board)
      (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7
              d1 d2 d3 d4 d5 d6 d7 e1 e2 e3 e4 e5 e6 e7 f1 f2 f3 f4 f5 f6 f7
              g1 g2 g3 g4 g5 g6 g7)

        (== board (list a1 a2 a3 a4 a5 a6 a7
                        b1 b2 b3 b4 b5 b6 b7
                        c1 c2 c3 c4 c5 c6 c7
                        d1 d2 d3 d4 d5 d6 d7
                        e1 e2 e3 e4 e5 e6 e7
                        f1 f2 f3 f4 f5 f6 f7
                        g1 g2 g3 g4 g5 g6 g7))

        (paralelize_seeds
          (list a1 a2 a3 a4 a5 a6 b1 b2 b3 b4 b5 b6 c1 c2 c3 c4 c5 c6
                d1 d2 d3 d4 d5 d6 e1 e2 e3 e4 e5 e6 f1 f2 f3 f4 f5 f6)
          (solutions_nQ (6Queens) 5))

        (initialize (list                   a7
                                            b7
                                            c7
                                            d7
                                            e7
                                            f7
                          g1 g2 g3 g4 g5 g6 g7))

        (fresh (→1 →2 →3 →4 →5 →6 →7 ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ↓7
                ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇗10 ⇗11
                ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9 ⇘10 ⇘11)

          (== →1 (list a1 a2 a3 a4 a5 a6 a7))
          (== →2 (list b1 b2 b3 b4 b5 b6 b7))
          (== →3 (list c1 c2 c3 c4 c5 c6 c7))
          (== →4 (list d1 d2 d3 d4 d5 d6 d7))
          (== →5 (list e1 e2 e3 e4 e5 e6 e7))
          (== →6 (list f1 f2 f3 f4 f5 f6 f7))
          (== →7 (list g1 g2 g3 g4 g5 g6 g7))
          (== ↓1 (list a1 b1 c1 d1 e1 f1 g1))
          (== ↓2 (list a2 b2 c2 d2 e2 f2 g2))
          (== ↓3 (list a3 b3 c3 d3 e3 f3 g3))
          (== ↓4 (list a4 b4 c4 d4 e4 f4 g4))
          (== ↓5 (list a5 b5 c5 d5 e5 f5 g5))
          (== ↓6 (list a6 b6 c6 d6 e6 f6 g6))
          (== ↓7 (list a7 b7 c7 d7 e7 f7 g7))
          (== ⇗1  (list b1 a2))
          (== ⇗2  (list c1 b2 a3))
          (== ⇗3  (list d1 c2 b3 a4))
          (== ⇗4  (list e1 d2 c3 b4 a5))
          (== ⇗5  (list f1 e2 d3 c4 b5 a6))
          (== ⇗6  (list g1 f2 e3 d4 c5 b6 a7))
          (== ⇗7  (list g2 f3 e4 d5 c6 b7))
          (== ⇗8  (list g3 f4 e5 d6 c7))
          (== ⇗9  (list g4 f5 e6 d7))
          (== ⇗10 (list g5 f6 e7))
          (== ⇗11 (list g6 f7))
          (== ⇘1  (list a6 b7))
          (== ⇘2  (list a5 b6 c7))
          (== ⇘3  (list a4 b5 c6 d7))
          (== ⇘4  (list a3 b4 c5 d6 e7))
          (== ⇘5  (list a2 b3 c4 d5 e6 f7))
          (== ⇘6  (list a1 b2 c3 d4 e5 f6 g7))
          (== ⇘7  (list b1 c2 d3 e4 f5 g6))
          (== ⇘8  (list c1 d2 e3 f4 g5))
          (== ⇘9  (list d1 e2 f3 g4))
          (== ⇘10  (list e1 f2 g3))
          (== ⇘11 (list f1 g2))

          (at_most_once_each (list →1 →2 →3 →4 →5 →6 →7 ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ↓7
                                   ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇗10 ⇗11
                                   ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9 ⇘10 ⇘11)))))))

; [check-equal? (solution_nQ (7Queens) 7)
;               '(_ Q _ _ _ _ _
;                   _ _ _ Q _ _ _
;                   Q _ _ _ _ _ _
;                   _ _ _ _ _ _ Q
;                   _ _ _ _ Q _ _
;                   _ _ Q _ _ _ _
;                   _ _ _ _ _ Q _)]
