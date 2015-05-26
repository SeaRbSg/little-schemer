#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")

(define (append° l s out)               ; 9
  (cond-e [(null° l) (≈ s out)]
          [(fresh (a d res)
             (cons° a d l)              ; deconstruct l
             (cons° a res out)          ; build (cons a res) => out
             (append° d s res))]))      ; always recurse last

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (x)                       ; 10
              (append° '(cake) '(tastes yummy) x)
              => '((cake tastes yummy)))

  (check-run 5 (x)                      ; 11 -- can't run run* w/ my version
             (fresh (y)
               (append° `(cake with ice . ,y) '(tastes yummy) x))
             => '((cake with ice tastes yummy)
                  (cake with ice _.0 tastes yummy)
                  (cake with ice _.0 _.1 tastes yummy)
                  (cake with ice _.0 _.1 _.2 tastes yummy)
                  (cake with ice _.0 _.1 _.2 _.3 tastes yummy)))

  (check-run1 (x)                       ; 13
              (fresh (y)
                (append° `(cake with ice . ,y) '(d t) x))
              => '((cake with ice d t)))

  ;; 14 is the same as my 11

  (check-run 5 (y)                      ; 17
             (fresh (x)
               (append° `(cake with ice . ,y) '(d t) x))
             => '(()
                  (_.0)
                  (_.0 _.1)
                  (_.0 _.1 _.2)
                  (_.0 _.1 _.2 _.3)))

  (check-run 5 (x)                      ; 20
             (fresh (y)
               (append° `(cake with ice . ,y) `(d t . ,y) x))
             => '((cake with ice d t)
                  (cake with ice _.0 d t  _.0)
                  (cake with ice _.0 _.1 d t _.0 _.1)
                  (cake with ice _.0 _.1 _.2 d t  _.0 _.1 _.2)
                  (cake with ice _.0 _.1 _.2 _.3 d t  _.0 _.1 _.2 _.3)))

  (check-run* (x)                       ; 21
              (fresh (z)
                (append° '(cake with ice cream) `(d t . ,z) x))
              => '((cake with ice cream d t . _.0)))

  (check-run 6 (x)                      ; 23
             (fresh (y)
               (append° x y '(cake with ice d t)))
             => '(()
                  (cake)
                  (cake with)
                  (cake with ice)
                  (cake with ice d)
                  (cake with ice d t)))


  (check-run 6 (y)                      ; 25
             (fresh (x)
               (append° x y '(cake with ice d t)))
             => '((cake with ice d t)
                  (with ice d t)
                  (ice d t)
                  (d t)
                  (t)
                  ()))

  (check-run 6 (r)                      ; 27
             (fresh (x y)
               (append° x y '(cake with ice d t))
               (≈ (list x y) r))
             => '((() (cake with ice d t))
                  ((cake)  (with ice d t))
                  ((cake with)  (ice d t))
                  ((cake with ice)  (d t))
                  ((cake with ice d)  (t))
                  ((cake with ice d t) ())))

  (check-run 7 (r)                      ; 29 -- hah, mine is perfect
             (fresh (x y)
               (append° x y '(cake with ice d t))
               (≈ (list x y) r))
             => '((() (cake with ice d t))
                  ((cake)  (with ice d t))
                  ((cake with)  (ice d t))
                  ((cake with ice)  (d t))
                  ((cake with ice d)  (t))
                  ((cake with ice d t) ())))

  (check-run 7 (x)                      ; 33
             (fresh (y z)
               (append° x y z))
             => '(()
                  (_.0)
                  (_.0 _.1)
                  (_.0 _.1 _.2)
                  (_.0 _.1 _.2 _.3)
                  (_.0 _.1 _.2 _.3 _.4)
                  (_.0 _.1 _.2 _.3 _.4 _.5)))

  (check-run 7 (y)                      ; 34
             (fresh (x z)
               (append° x y z))
             => '(_.0 _.0 _.0 _.0 _.0 _.0 _.0))

  (check-run 7 (r)                      ; 37
             (fresh (x y z)
               (append° x y z)
               (≈ (list x y z) r))
             => '((()                        _.0 _.0)
                  ((_.0)                     _.1 (_.0 . _.1))
                  ((_.0 _.1)                 _.2 (_.0 _.1 . _.2))
                  ((_.0 _.1 _.2)             _.3 (_.0 _.1 _.2 . _.3))
                  ((_.0 _.1 _.2 _.3)         _.4 (_.0 _.1 _.2 _.3 . _.4))
                  ((_.0 _.1 _.2 _.3 _.4)     _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
                  ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))


  (define (unwrap° x out)               ; 45 -- with mods
    (cond-e [(≈ x out)]
            [(pair° x) (fresh (a)
                         (car° x a)
                         (unwrap° a out))]))

  (check-run* (x)                       ; 46
              (unwrap° '(((pizza))) x)
              => '((((pizza))) ((pizza)) (pizza) pizza))

  (define (flatten° s out)              ; 59
    (cond-e [(null° s) (≈ s out)]
            [(pair° s) (fresh (a d res1 res2)
                         (cons° a d s)
                         (flatten° a res1)
                         (flatten° d res2)
                         (append° res1 res2 out))]
            [(cons° s '() out)]))

  (check-run1 (x)                       ; 60
              (flatten° '((a b) c) x)
              => '((a b c)))

  (check-run1 (x)                       ; 61
              (flatten° '(a (b c)) x)
              => '((a b c)))

  (check-run* (x)                       ; 62
              (flatten° '(a) x)
              => '((a)                  ; normal
                   (a ())               ; normal + d null°
                   ((a))))              ; normal + a null°

  (check-run* (x)
              (flatten° '((a b) c) x)
              => '((a b c)
                   (a b c ())
                   (a b (c))
                   (a b () c)
                   (a b () c ())
                   (a b () (c))
                   (a (b) c)
                   (a (b) c ())
                   (a (b) (c))
                   ((a b) c)
                   ((a b) c ())
                   ((a b) (c))
                   (((a b) c))))

  ;; nope... bozo bit flipped... wtf

  (define (flattenrev° s out)              ; 59
    (cond-e [(cons° s '() out)]
            [(null° s) (≈ s out)]
            [(fresh (a d res1 res2)
               (cons° a d s)
               (flattenrev° a res1)
               (flattenrev° d res2)
               (append° res1 res2 out))]))


  (check-run 2 (x)
             (flattenrev° x '(a b c))
             => '((a b . c)
                  (a b c . ())))

  ;;; WTF
  ;;
  ;; I honestly think the code / book let us down here. There should
  ;; only be one value that satisfies (flatten° '(a) x): '(a). How it
  ;; comes to any other conclusions is beyond me.

  )
