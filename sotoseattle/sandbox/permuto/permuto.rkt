#lang racket
(require "../../basic_defs.rkt")
(require "../../../lib/mk.rkt")
(require rackunit)
(require racket/list)
(require racket/set)

; Given a value and a list of fresh vars, give me all combinations of value in list
(define conde_list
  (lambda (val fl o)
    (fresh (x y a b)
      (conso a b o)
      (conso x y fl)
      (conde
        [(== val a) (== y b)]
        [(conde_list val y b)]))))

; Given a list, give me all permutations
(define permuto
  (lambda (l o)
    (letrec
      ((freshlist (lambda (l o) ; make a twin list of fresh variables
                    (conde
                      [(nullo l) (nullo o)]
                      [(fresh (y b)
                         (cdro l y)
                         (cdro o b)
                         (freshlist y b))])))
       (condify (lambda (l fl o) ; conde for all vals of l in positions of fl
                  (conde
                    [(nullo l)]
                    [(fresh (x y)
                       (conso x y l)
                       (conde_list x fl o)
                       (condify y fl o))]))))
      (fresh (fl)
        (freshlist l fl)
        (condify l fl o)))))

(define expected
  (lambda ()
    '((1 2 3 4) (1 2 4 3) (1 3 2 4) (1 4 2 3) (1 3 4 2) (1 4 3 2)
      (2 1 3 4) (2 1 4 3) (3 1 2 4) (4 1 2 3) (3 1 4 2) (4 1 3 2)
      (2 3 1 4) (2 4 1 3) (3 2 1 4) (4 2 1 3) (3 4 1 2) (4 3 1 2)
      (2 3 4 1) (2 4 3 1) (3 2 4 1) (4 2 3 1) (3 4 2 1) (4 3 2 1))))

(check-true (set=? (permutations '(1 2 3 4)) (expected)))

[check-equal?
  (run* (q) (permuto '(1 2 3 4) q))
  (expected)]

