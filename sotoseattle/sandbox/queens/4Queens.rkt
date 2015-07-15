#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require rackunit)

(provide nono at_most_once at_most_once_each initialize count solution_nQ solutions_nQ 4Queens)

(define (nono l)
  (conde
    [(nullo l)]
    [(fresh (a d)
       (conso a d l)
       (ifa (== a 'Q) u# (nono d)))]))

(define at_most_once
  (lambda (l)
    (fresh (a d)
      (conso a d l)
      (conda
        [(nullo d) s#]
        [(== a 'Q) (nono d)]
        [else (at_most_once d)]))))

(define at_most_once_each
  (lambda (listas)
    (conde
      [(nullo listas)]
      [(fresh (a d)
         (conso a d listas)
         (at_most_once a)
         (at_most_once_each d))])))

(define initialize
  (lambda (board)
    (conde
      [(nullo board)]
      [(fresh (a d)
         (conso a d board)
         (conde [(== a 'Q)] [(== a '_)])
         (initialize d))])))

(define count
  (lambda (l)
    (cond
      [(null? l) 0]
      [(equal? (car l) 'Q) (+  1 (count (cdr l)))]
      [else (count (cdr l))])))

(define solution_nQ
  (lambda (l n)
    (cond
      [(null? l) #f]
      [(eq? (count (car l)) n) (car l)]
      [(solution_nQ (cdr l) n)])))

(define solutions_nQ
  (lambda (l n)
    (cond
      [(null? l) '()]
      [(eq? (count (car l)) n) (cons (car l) (solutions_nQ (cdr l) n))]
      [(solutions_nQ (cdr l) n)])))

(define 4Queens
  (lambda ()
    (run* (board)
      (fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
        (== board (list a1 a2 a3 a4
                        b1 b2 b3 b4
                        c1 c2 c3 c4
                        d1 d2 d3 d4))
        (initialize board)
        (fresh (→1 →2 →3 →4 ↓1 ↓2 ↓3 ↓4 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5)
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
          (at_most_once_each (list →1 →2 →3 →4 ↓1 ↓2 ↓3 ↓4 ⇗1 ⇗2 ⇗3 ⇗4 ⇗5 ⇘1 ⇘2 ⇘3 ⇘4 ⇘5)))))))

; [check-equal? (solution_nQ (4Queens) 4) '(_ Q _ _
;                                           _ _ _ Q
;                                           Q _ _ _
;                                           _ _ Q _)]

