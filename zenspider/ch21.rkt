#lang racket/base

(require "lib/reasonable.rkt")

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (q)                       ; 10
              %u
              => none)

  (check-run* (q)                       ; 11
              (≈ #t q)
              => '(#t))

  (check-run* (q)                       ; 12
              %u
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 13
              %s
              (≈ #t q)
              => '(#t))

  (check-run* (r)                       ; 15
              %s
              (≈ 'corn r)
              => '(corn))

  (check-run* (r)                       ; 17
              %u
              (≈ 'corn r)
              => none)

  (check-run* (x)                       ; 22
              (let ([x #f])
                (≈ #t x))
              => none)

  (check-run* (q)                       ; 23
              (fresh (x)
                (≈ #t x)
                (≈ #t q))
              => '(#t))

;;;; The Law of Fresh
  ;;
  ;; If x is fresh, then (≈ v x) succeeds and associates x with v.

  (check-run* (q)                       ; 26
              (fresh (x)
                (≈ x #t)
                (≈ #t q))
              => '(#t))

;;;; The Law of ≈
  ;;
  ;; (≈ v w) is the same as (≈ w v)

  (check-run* (q)                       ; 28
              %s
              => '(_.0))

  (check-run* (r)                       ; 30
              (fresh (x y)
                (≈ (list x y) r))
              => '((_.0 _.1)))

  (check-run* (r)                       ; 31
              (fresh (t u)
                (≈ (list t u) r))
              => '((_.0 _.1)))

  (check-run* (r)                       ; 33
              (fresh (x)
                (let ([y x])
                  (fresh (x)
                    (≈ (list x y x) r))))
              => '((_.0 _.1 _.0)))

  (check-run* (q)                       ; 34
              (≈ #f q)
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 35
              (≈ #f q)
              (≈ #f q)
              => '(#f))

  (check-run* (q)                       ; 36
              (let ([x q])
                (≈ #t x))
              => '(#t))

  (check-run* (r)                       ; 37
              (fresh (x)
                (≈ x r))
              => '(_.0))

  (check-run* (q)                       ; 38
              (fresh (x)
                (≈ #t x)
                (≈ x q))
              => '(#t))

  (check-run* (q)                       ; 39
              (fresh (x)
                (≈ x q)
                (≈ #t x))
              => '(#t))

  ;; can't actually do this one, because #f isn't a lambda
  ;; (check-run* (q)                    ; 41
  ;;             (cond-e [#f %s]
  ;;                     [else #f])
  ;;             => #f)

  ;; closest I can get to a working example -- the lambda is basically %u
  (check-run* (q)                       ; 41
              (cond-e [(lambda (_) #f) %s]
                      [else (lambda (_) #f)])
              => none)

  (check-equal? ((cond-e [%u %s]        ; 44.1
                         [else %u]) 'does-not-matter)
                #f)

  (check-run* (q)                       ; 44.2
              (cond-e [%u %s]
                      [else %u])
              => none)

  (check-equal? ((cond-e [%u %u]        ; 45.1
                         [else %s]) 'success)
                'success)

  (check-run* (q)                       ; 45.2
              (cond-e [%u %u]
                      [else %s])
              => '(_.0))

  ;; raw result == (first-binding . lambda-to-continue-stream)
  (let ([result ((cond-e [%s %s]        ; 46
                         [else %u]) 'success)])
    (check-equal? (car result)
                  'success)

    (check-equal? ((cdr result))
                  #f))

  (check-run* (x)                       ; 47
              (cond-e [(≈ 'olive x) %s]
                      [(≈ 'oil x)   %s]
                      [else %u])
              => '(olive oil))

;;;; The Law of cond-e
  ;;
  ;; To get more values from cond-e, pretend that the successful
  ;; cond-e line has failed, refreshing all variables that got an
  ;; association from that line.

  (check-run 1 (x)                      ; 49
             (cond-e [(≈ 'olive x) %s]
                     [(≈ 'oil x)   %s]
                     [else %u])
             => '(olive))

  (check-run* (x)                       ; 50
              (cond-e [(≈ 'virgin x) %u]
                      [(≈ 'olive x)  %s]
                      [%s            %s]
                      [(≈ 'oil x)    %s]
                      [else %u])
              => '(olive _.0 oil))

  (check-run 2 (x)                      ; 52
             (cond-e [(≈ 'olive x) %s]
                     [(≈ 'oil x)   %s]
                     [else %u])
             => '(olive oil))

  (check-run* (r)                       ; 53
              (fresh (x y)
                (≈ 'split x)
                (≈ 'pea y)
                (≈ (list x y) r))
              => '((split pea)))

  (check-run* (r)                       ; 54
              (fresh (x y)
                (cond-e [(≈ 'split x) (≈ 'pea y)]
                        [(≈ 'navy x) (≈ 'bean y)]
                        [else %u])
                (≈ (list x y) r))
              => '((split pea) (navy bean)))

  (check-run* (r)                       ; 55
              (fresh (x y)
                (cond-e [(≈ 'split x) (≈ 'pea y)]
                        [(≈ 'navy x) (≈ 'bean y)]
                        [else %u])
                (≈ (list x y 'soup) r))
              => '((split pea soup) (navy bean soup)))

  (define (teacup-o x)
    (cond-e [(≈ 'tea x) %s]
            [(≈ 'cup x) %s]
            [else %u]))

  (check-run* (x)                       ; 56
              (teacup-o x)
              => '(tea cup))

  (check-run* (r)                       ; 57
              (fresh (x y)
                (cond-e [(teacup-o x) (≈ #t y) %s]
                        [(≈ #f x) (≈ #t y)]
                        [else %u])
                (≈ (list x y) r))
              => '((tea #t)
                   (cup #t)
                   (#f #t)))

  (check-run* (r)                       ; 58
              (fresh (x y z)
                (cond-e [(≈ y x) (fresh (x) (≈ z x))]
                        [(fresh (x) (≈ y x)) (≈ z x)]
                        [else %u])
                (≈ (list y z) r))
              => '((_.0 _.1)
                   (_.0 _.1)))

  (check-run* (r)                       ; 59
              (fresh (x y z)
                (cond-e [(≈ y x) (fresh (x) (≈ z x))]
                        [(fresh (x) (≈ y x)) (≈ z x)]
                        [else %u])
                (≈ #f x)
                (≈ (list y z) r))
              => '((#f _.0)
                   (_.0 #f)))

  (check-run* (q)                       ; 60
              (let ([a (≈ #t q)]
                    [b (≈ #f q)])
                b)
              => '(#f))

  (check-run* (q)
              (let ([a (≈ #t q)]
                    [b (fresh (x)
                         (≈ x q)
                         (≈ #f x))]
                    [c (cond-e [(≈ #t q) %s]
                               [else (≈ #f q)])])
                b)
              => '(#f)))
