#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require "4Queens.rkt")
(require "5Queens.rkt")
(require rackunit)

(provide 6Queens)

(define 6Queens
  (lambda ()
    (run* (board)
      (fresh (a1 a2 a3 a4 a5 a6 b1 b2 b3 b4 b5 b6 c1 c2 c3 c4 c5 c6 d1 d2 d3 d4 d5 d6 e1 e2 e3 e4 e5 e6 f1 f2 f3 f4 f5 f6)
        (== board (list a1 a2 a3 a4 a5 a6
                        b1 b2 b3 b4 b5 b6
                        c1 c2 c3 c4 c5 c6
                        d1 d2 d3 d4 d5 d6
                        e1 e2 e3 e4 e5 e6
                        f1 f2 f3 f4 f5 f6))

        (paralelize_seeds
          (list a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5 d1 d2 d3 d4 d5 e1 e2 e3 e4 e5)
          (concho (solutions_nQ (5Queens) 5) (solutions_nQ (5Queens) 4)))

        (initialize (list                a6
                                         b6
                                         c6
                                         d6
                                         e6
                          f1 f2 f3 f4 f5 f6))

        (fresh (→1 →2 →3 →4 →5 →6 ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9)
          (== →1 (list a1 a2 a3 a4 a5 a6))
          (== →2 (list b1 b2 b3 b4 b5 b6))
          (== →3 (list c1 c2 c3 c4 c5 c6))
          (== →4 (list d1 d2 d3 d4 d5 d6))
          (== →5 (list e1 e2 e3 e4 e5 e6))
          (== →6 (list f1 f2 f3 f4 f5 f6))
          (== ↓1 (list a1 b1 c1 d1 e1 f1))
          (== ↓2 (list a2 b2 c2 d2 e2 f2))
          (== ↓3 (list a3 b3 c3 d3 e3 f3))
          (== ↓4 (list a4 b4 c4 d4 e4 f4))
          (== ↓5 (list a5 b5 c5 d5 e5 f5))
          (== ↓6 (list a6 b6 c6 d6 e6 f6))
          (== ⇗1  (list b1 a2))
          (== ⇗2  (list c1 b2 a3))
          (== ⇗3  (list d1 c2 b3 a4))
          (== ⇗4  (list e1 d2 c3 b4 a5))
          (== ⇗5  (list f1 e2 d3 c4 b5 a6))
          (== ⇗6  (list f2 e3 d4 c5 b6))
          (== ⇗7  (list f3 e4 d5 c6))
          (== ⇗8  (list f4 e5 d6))
          (== ⇗9  (list f5 e6))
          (== ⇘1  (list a5 b6))
          (== ⇘2  (list a4 b5 c6))
          (== ⇘3  (list a3 b4 c5 d6))
          (== ⇘4  (list a2 b3 c4 d5 e6))
          (== ⇘5  (list a1 b2 c3 d4 e5 f6))
          (== ⇘6  (list b1 c2 d3 e4 f5))
          (== ⇘7  (list c1 d2 e3 f4))
          (== ⇘8  (list d1 e2 f3))
          (== ⇘9  (list e1 f2))

          (at_most_once_each (list →1 →2 →3 →4 →5 →6 ↓1 ↓2 ↓3 ↓4 ↓5 ↓6 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇗8 ⇗9 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7 ⇘8 ⇘9))
          )))))

; (solutions_nQ (6Queens) 6)

; [check-equal? (solution_nQ (6Queens) 6) '(_ Q _ _ _ _
;                                           _ _ _ Q _ _
;                                           _ _ _ _ _ Q
;                                           Q _ _ _ _ _
;                                           _ _ Q _ _ _
;                                           _ _ _ _ Q _)]

