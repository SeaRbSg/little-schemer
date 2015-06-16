#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")

(define (bit-nand° x y r)
  (cond-e
   ((≈ 0 x) (≈ 0 y) (≈ 1 r))
   ((≈ 1 x) (≈ 0 y) (≈ 1 r))
   ((≈ 0 x) (≈ 1 y) (≈ 1 r))
   ((≈ 1 x) (≈ 1 y) (≈ 0 r))))

(define (bit-xor° x y r)                ; 5
  (fresh (s t u)
    (bit-nand° x y s)
    (bit-nand° x s t)
    (bit-nand° s y u)
    (bit-nand° t u r)))

(define (bit-not° x r)
  (bit-nand° x x r))

(define (bit-and° x y r)
  (fresh (s)
    (bit-nand° x y s)
    (bit-not° s r)))

(define (half-adder° x y r c)           ; 12
  (fresh ()                             ; replaced all with fresh
    (bit-xor° x y r)
    (bit-and° x y c)))

(define (full-adder° ci x y r c)
  (fresh (w xy wz)
    (half-adder°  x y w xy)
    (half-adder° ci w r wz)
    (bit-xor° xy wz c)))

(define (build-num n)                   ; 43
  (cond [(zero? n) '()]
        [(even? n) (cons 0 (build-num (quotient n 2)))]
        [else      (cons 1 (build-num (quotient n 2)))]))

(define pos° pair°)                     ; 80 -- bite me

;; (define (>1° n)                      ; 86
;;   (fresh (a ad dd)
;;     (≈ n (cons a (cons ad dd)))))

(define (>1° n)                         ; 86
  (fresh (a d)
    (cons° a d n)
    (pair° d)))

(define (gen-adder° d n m r)
  (fresh (a b c e x y z)
    (cons° a x n)
    (cons° b y m) (pos° y)
    (cons° c z r) (pos° z)
    (all-i (full-adder° d a b c e)
           (adder° e x y z))))

(define (adder° d n m r)                ; 118
  (cond-i [(≈ 0 d) (≈ '() m) (≈ n r)]
          [(≈ 0 d) (≈ '() n) (pos° m) (≈ m r)]
          [(≈ 1 d) (≈ `() m)          (adder° 0 n '(1) r)]
          [(≈ 1 d) (≈ `() n) (pos° m) (adder° 0 '(1) m r)]
          [(≈ '(1) n) (≈ '(1) m) (fresh (a c)
                                   (≈ (list a c) r)
                                   (full-adder° d 1 1 a c))]
          [(≈ '(1) n) (gen-adder° d n m r)]
          [(≈ '(1) m) (>1° n) (>1° r) (adder° d '(1) n r)]
          [(>1° n) (gen-adder° d n m r)]))

(define (+° n m k)
  (adder° 0 n m k))

(define (-° n m k)
  (adder° 0 n k m))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (define-syntax check-truth
    (syntax-rules ()
      [(_ fn exp) (check-truth fn (x y z) exp)] ; 2 in, 1 out == default
      [(_ fn (vars ...) exp) (check-run* (r)
                                         (fresh (vars ...)
                                           (fn vars ...)
                                           (≈ r (list vars ...)))
                                         => exp)]))

  (define-syntax check-run?
    (syntax-rules ()
      [(_ stmt exp) (check-run* (r) stmt (≈ r #t) => exp)]))
  (define-syntax check-good
    (syntax-rules ()
      [(_ stmt) (check-run? stmt '(#t))]))
  (define-syntax check-bad
    (syntax-rules ()
      [(_ stmt) (check-run? stmt none)]))

  (check-truth bit-nand° '((0 0 1)
                           (1 0 1)
                           (0 1 1)
                           (1 1 0)))

  (check-truth bit-xor° '((0 0 0)       ; 9
                          (1 0 1)
                          (0 1 1)
                          (1 1 0)))

  (check-truth bit-not° (x o) '((0 1)
                                (1 0)))

  (check-truth bit-and° '((0 0 0)       ; 11
                          (1 0 0)
                          (0 1 0)
                          (1 1 1)))

  (check-truth half-adder° (x y r c) '((0 0 0 0) ; 13
                                       (1 0 1 0)
                                       (0 1 1 0)
                                       (1 1 0 1)))

  (check-truth full-adder° (ci x y r c) '((0 0 0 0 0) ; 15
                                          (1 0 0 1 0)
                                          (0 1 0 1 0)
                                          (1 1 0 0 1)
                                          (0 0 1 1 0)
                                          (1 0 1 0 1)
                                          (0 1 1 0 1)
                                          (1 1 1 1 1)))

  (check-equal? '() (build-num 0))
  (check-equal? '(1) (build-num 1))
  (check-equal? '(0 1) (build-num 2))
  (check-equal? '(1 1) (build-num 3))
  (check-equal? '(0 0 1) (build-num 4))

  (check-good (pos° '(0 1 1)))          ; 80
  (check-good (pos° '(1)))              ; 81
  (check-bad  (pos° '()))               ; 82

  (check-run* (q)                       ; 83
              (pos° q)
              => '((_.0 . _.1)))

  (check-good (>1° '(0 1 1)))           ; 86
  (check-good (>1° '(0 1)))             ; 87
  (check-bad  (>1° '(1)))               ; 88
  (check-bad  (>1° '()))                ; 89

  (check-run* (q)                       ; 90
              (>1° q)
              => '((_.0 _.1 . _.2)))


  (check-run 3 (s)                      ; 97
             (fresh (x y r)
               (adder° 0 x y r)
               (≈ (list x y r) s))
             => '((_.0 () _.0)
                  (() (_.0 . _.1) (_.0 . _.1))
                  ((1) (1) (0 1))))

  ;; I am NOT doing a run 22. Fuck you.

  (check-run 9 (s)                      ; 101-ish
             (fresh (x y r)
               (adder° 0 x y r)
               (≈ (list x y r) s))
             => '((_.0 () _.0)
                  (() (_.0 . _.1) (_.0 . _.1))
                  ((1) (1) (0 1))
                  ((1) (0 _.0 . _.1) (1 _.0 . _.1))
                  ((0 _.0 . _.1) (1) (1 _.0 . _.1))
                  ((1) (1 1) (0 0 1))
                  ((0 1) (0 1) (0 0 1))

                  ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))

                  ((1 1) (1) (0 0 1))

                  ))

  (check-run* (s)                       ; 120
              (gen-adder° 1 '(0 1 1) '(1 1) s)
              => '((0 1 0 1)))

  (check-run* (s)                       ; 126
              (fresh (x y)
                (adder° 0 x y '(1 0 1)))
              => '(((1 0 1) ())
                   (() (1 0 1))
                   ((1) (0 0 1))
                   ((0 0 1) (1))
                   ((1 1) (0 1))
                   ((0 1) (1 1))))      ; fails with (_.0 _.0 _.0 _.0 _.0 _.0)

  (check-run* (s)                       ; 128
              (fresh (x y)
                (+° x y '(1 0 1)))
              => '(((1 0 1) ())
                   (() (1 0 1))
                   ((1) (0 0 1))
                   ((0 0 1) (1))
                   ((1 1) (0 1))
                   ((0 1) (1 1))))      ; fails with (_.0 _.0 _.0 _.0 _.0 _.0)

  (check-run* (q)                       ; 131
              (-° '(0 0 0 1) '(1 0 1) q)
              => '((1 1)))              ; fails with ()

  (check-run* (q)                       ; 132
              (-° '(0 1 1) '(0 1 1) q)
              => '(()))

  (check-run* (q)                       ; 133
              (-° '(0 1 1) '(0 0 0 1) q)
              => '(()))                 ; fails with (0 1)

  'done)
