#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (x)                       ; 1
              (cond-a [%u %s]
                      [else %u])
              => none)

  (check-run* (x)                       ; 2
              (cond-a [%u %s]
                      [else %s])
              => '(_.0))

  (check-run* (x)                       ; 3
              (cond-a [%s %u]
                      [else %s])
              => none)

  (check-run* (x)                       ; 4
              (cond-a [%s %s]
                      [else %u])
              => '(_.0))

  (check-run* (x)                       ; 5
              (cond-a [(≈ 'olive x) %s]
                      [(≈ 'oil x) %s]
                      [else %u])
              => '(olive))

  ;;; The law of cond-a
  ;;
  ;; If the question of a cond-a line succeeds, pretend that the
  ;; remaining cond-a lines have been replaced by a single [else %u].

  (check-run* (x)                       ; 7
              (cond-a [(≈ 'virgin x) %u]
                      [(≈ 'olive x) %s]
                      [(≈ 'oil x) %s]
                      [else %u])
              => none)

  (check-run* (q)                       ; 8
              (fresh (x y)
                (≈ 'split x)
                (≈ 'pea y)
                (cond-a [(≈ 'split x) (≈ x y)]
                        [else %s]))
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 9
              (fresh (x y)
                (≈ 'split x)
                (≈ 'pea y)
                (cond-a [(≈ x y) (≈ 'split x)]
                        [else %s]))
              (≈ #t q)
              => '(#t))

  (define (not-pasta° x)
    (cond-a [(≈ 'pasta x) %u]
            [else %s]))

  (check-run* (x)                       ; 11 -- I don't understand how this works
              (cond-a [(not-pasta° x) %u]
                      [else (≈ 'spaghetti x)])
              => '(spaghetti))

  (check-run* (x)                       ; 12
              (≈ 'spaghetti x)
              (cond-a [(not-pasta° x) %u]
                      [else (≈ 'spaghetti x)])
              => none)

  ;;; The Third Commandment
  ;;
  ;; If prior to determining the question of a cond-a line a variable
  ;; is fresh, it must remain fresh in the question of that line
  ;;
  ;; Reworded:
  ;;
  ;; If a variable is fresh prior to determining the question of a
  ;; cond-a line, it must remain fresh in the question of that line.

  

  'done)
