#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")
(require "ch23.rkt")

;;; The Second Commandment
;;
;; To transform a function whose value is not a boolean into a
;; function whose value is a goal, add an extra argument to hald its
;; value, replace cond with cond-e, and unnest each question and
;; answer.

(define (mem° x l out)                  ; 7
  (cond-e [(eq-car° l x) (≈ l out)]
          [else (fresh (d)
                  (cdr° l d)
                  (mem° x d out))]))

(define (rember° x l out)               ; 24
  (cond-e [(null° l) (≈ '() out)]
          [(eq-car° l x) (cdr° l out)]
          [else (fresh (res a d)
                  (cons° a d l)         ; deconstruct l
                  (cons° a res out)     ; build (cons a res) => out
                  (rember° x d res))])) ; always recurse last

(define (surprise° s)                   ; 68
  (rember° s '(a b c) '(a b c)))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run1 (out)                     ; 10
              (mem° 'tofu '(a b tofu d tofu e) out)
              => '((tofu d tofu e)))

  (check-run1 (out)                     ; 11
              (fresh (x)
                (mem° 'tofu `(a b ,x d tofu e) out))
              => '((tofu d tofu e)))

  (check-run* (r)                       ; 12
              (mem° r '(a b tofu d tofu e) '(tofu d tofu e))
              => '(tofu))

  (check-run* (q)                       ; 13
              (mem° 'tofu '(tofu e) '(tofu e))
              (≈ #t q)
              => '(#t))

  (check-run* (q)                       ; 14
              (mem° 'tofu '(tofu e) '(tofu))
              (≈ #t q)
              => none)

  (check-run* (x)                       ; 15
              (mem° 'tofu '(tofu e) `(,x e))
              => '(tofu))

  (check-run* (x)                       ; 16
              (mem° 'tofu '(tofu e) '(peas x))
              => none)

  (check-run* (out)                     ; 17
              (fresh (x)
                (mem° 'tofu `(a b ,x d tofu e) out))
              => '((tofu d tofu e)
                   (tofu e)))

  (check-run 4 (z)                      ; 18
             (fresh (u)
               (mem° 'tofu `(a b tofu d tofu e . ,z) u))
             => '(
                  _.0
                  _.0
                  (tofu . _.0)
                  (_.0 tofu . _.1)))

  (check-run1 (out)
              (fresh (y)
                (rember° 'peas `(a b ,y d peas e) out))
              => '((a b d peas e)))

  (check-run* (out)                     ; 31
              (fresh (y z)
                (rember° y `(a b ,y d ,z e) out))
              => '((b a d _.0 e)
                   (a b d _.0 e)
                   (a b d _.0 e)
                   (a b d _.0 e)
                   (a b _.0 d e)
                   (a b e d _.0)
                   (a b _.0 d _.1 e)))

  (check-run* (r)                       ; 49
              (fresh (y z)
                (rember° y `(,y d ,z e) `(,y d e))
                (≈ (list y z) r))
              => '((d d)
                   (d d)
                   (_.0 _.0)
                   (e e)))

  (check-run 13 (w)
             (fresh (y z out)
               (rember° y `(a b ,y d ,z . ,w) out))
             => '(_.0                   ; remove 'a, any tail
                  _.0                   ; remove 'b, any tail
                  _.0                   ; remove  y, any tail
                  _.0                   ; remove 'd, any tail
                  _.0                   ; remove 'z, any tail
                  ()                    ; null° l
                  (_.0 . _.1)           ; on tail, non-null, cons° out
                  (_.0)                 ; on tail, non-null, cons° out, null res
                  (_.0 _.1 . _.2)       ; etc
                  (_.0 _.1)
                  (_.0 _.1 _.2 . _.3)
                  (_.0 _.1 _.2)
                  (_.0 _.1 _.2 _.3 . _.4)))

  (check-run* (r)                       ; 69
              (≈ r 'd)
              (surprise° r)
              => '(d))

  (check-run* (r)                       ; 70
              (surprise° r)
              => '(_.0))

  (check-run* (r)                       ; 71 -- makes no sense
              (surprise° r)
              (≈ r 'b)
              => '(b))

  (check-run* (r)                       ; 72 -- makes no sense
              (≈ r 'b)
              (surprise° r)
              => '(b))


  )
