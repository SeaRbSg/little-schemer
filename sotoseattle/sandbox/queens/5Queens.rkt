#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require "4Queens.rkt")
(require rackunit)

(provide paralelize_seeds concho 5Queens)

(define paralelize_seeds
  (lambda (l sols)
    (fresh (a d)
      (conso a d sols)
      (conde
        [(== l a)]
        [(paralelize_seeds l d)]))))

(define concho
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(cons (car l1) (concho (cdr l1) l2))])))

(define 5Queens
  (lambda ()
    (run* (board)
      (fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5 d1 d2 d3 d4 d5 e1 e2 e3 e4 e5 f1 f2 f3 f4 f5)
        (== board (list a1 a2 a3 a4 a5
                        b1 b2 b3 b4 b5
                        c1 c2 c3 c4 c5
                        d1 d2 d3 d4 d5
                        e1 e2 e3 e4 e5))

        (paralelize_seeds (list a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
                          (solutions_nQ (4Queens) 2))
                          ; (concho (solutions_nQ (4Queens) 2)
                          ;         (concho (solutions_nQ (4Queens) 3) (solutions_nQ (4Queens) 4))))

        (initialize (list             a5
                                      b5
                                      c5
                                      d5
                          e1 e2 e3 e4 e5))

        (fresh (→1 →2 →3 →4 →5 ↓1 ↓2 ↓3 ↓4 ↓5 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7)
          (== →1 (list a1 a2 a3 a4 a5))
          (== →2 (list b1 b2 b3 b4 b5))
          (== →3 (list c1 c2 c3 c4 c5))
          (== →4 (list d1 d2 d3 d4 d5))
          (== →5 (list e1 e2 e3 e4 e5))
          (== ↓1 (list a1 b1 c1 d1 e1))
          (== ↓2 (list a2 b2 c2 d2 e2))
          (== ↓3 (list a3 b3 c3 d3 e3))
          (== ↓4 (list a4 b4 c4 d4 e4))
          (== ↓5 (list a5 b5 c5 d5 e5))
          (== ⇗1  (list b1 a2))
          (== ⇗2  (list c1 b2 a3))
          (== ⇗3  (list d1 c2 b3 a4))
          (== ⇗4  (list e1 d2 c3 b4 a5))
          (== ⇗5  (list e2 d3 c4 b5))
          (== ⇗6  (list e3 d4 c5))
          (== ⇗7  (list e4 d5))
          (== ⇘1  (list a4 b5))
          (== ⇘2  (list a3 b4 c5))
          (== ⇘3  (list a2 b3 c4 d5))
          (== ⇘4  (list a1 b2 c3 d4 e5))
          (== ⇘5  (list b1 c2 d3 e4))
          (== ⇘6  (list c1 d2 e3))
          (== ⇘7  (list d1 e2))

          (at_most_once_each (list →1 →2 →3 →4 →5 ↓1 ↓2 ↓3 ↓4 ↓5 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇗6 ⇗7 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5 ⇘6 ⇘7)))))))

; [check-equal? (solution_nQ (5Queens) 5) '(Q _ _ _ _
;                                           _ _ Q _ _
;                                           _ _ _ _ Q
;                                           _ Q _ _ _
;                                           _ _ _ Q _)]

