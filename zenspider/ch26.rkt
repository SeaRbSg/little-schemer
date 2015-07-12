#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")
(require "ch21.rkt")                    ; teacup°

;;; The Law of cond-i
;;
;; cond-i behaves like cond-e, except that its values are interleaved.

(define (any° g)                        ; 1
  (cond-e [g]
          [else (any° g)]))

(define never° (any° %u))               ; 4

(define always° (any° %s))              ; 7

(define (sal° g)                        ; 12
  (cond-e [%s]
          [g]))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  ;; (check-run1 (q)                    ; 6
  ;;             never°
  ;;             (≈ #t q)
  ;;             => none)

  (check-run1 (q)                       ; 7
              always°
              (≈ #t q)
              => '(#t))

  (check-run 5 (q)                      ; 10
             always°
             (≈ #t q)
             => '(#t #t #t #t #t))


  (check-run1 (q)                       ; 14
              (sal° never°)
              (≈ #t q)
              => '(#t))

  ;; (check-run1 (q)                    ; 16
  ;;             (sal° never°)
  ;;             %u
  ;;             (≈ #t q)
  ;;             => 'bad)

  ;; (check-run1 (q)                    ; 17
  ;;             always°
  ;;             %u
  ;;             (≈ #t q)
  ;;             => 'bad)

  ;; (check-run1 (q)                    ; 18
  ;;             (cond-e [(≈ #f q) always°]
  ;;                     [(any° (≈ #t q))])
  ;;             (≈ #t q)
  ;;             => 'bad)

  (check-run1 (q)                       ; 19
              (cond-i [(≈ #f q) always°]
                      [(≈ #t q)])
              (≈ #t q)
              => '(#t))

  ;; (check-run 2 (q)                   ; 20
  ;;            (cond-i [(≈ #f q) always°]
  ;;                    [(≈ #t q)])
  ;;            (≈ #t q)
  ;;            => 'bad)

  (check-run 5 (q)                      ; 21
             (cond-i [(≈ #f q) always°]
                     [else (any° (≈ #t q))])
             (≈ #t q)
             => '(#t #t #t #t #t))

  (check-run 5 (r)                      ; 24
             (cond-i [(teacup° r)]
                     [(≈ #f r)])
             => '(tea #f cup))

  (check-run 5 (q)                      ; 27
             (cond-i [(≈ #f q) always°]
                     [(≈ #t q) always°])
             (≈ #t q)
             => '(#t #t #t #t #t))

  (check-run 5 (q)                      ; 28
             (cond-e [always°]
                     [never°])
             (≈ #t q)
             => '(#t #t #t #t #t))

  ;; (check-run1 (q)                    ; 30
  ;;             (cond-i [always°]
  ;;                     [never°])
  ;;             (≈ #t q)
  ;;             => 'bad)

  ;; (check-run1 (q)                    ; 31
  ;;             (all (cond-e [(≈ #f q)]
  ;;                          [(≈ #t q)])
  ;;                  always°)
  ;;             (≈ #t q)
  ;;             => 'bad)

  (check-run1 (q)                       ; 32
              (all-i (cond-e [(≈ #f q)]
                             [(≈ #t q)])
                     always°)
              (≈ #t q)
              => '(#t))

  (check-run 5 (q)                      ; 33
             (all-i (cond-e [(≈ #f q)]
                            [(≈ #t q)])
                    always°)
             (≈ #t q)
             => '(#t #t #t #t #t))

  (check-run 5 (q)                      ; 34
             (all-i (cond-e [(≈ #t q)]
                            [(≈ #f q)])
                    always°)
             (≈ #t q)
             => '(#t #t #t #t #t))

  (check-run 5 (q)                      ; 36
             (all (cond-e [%s %s]
                          [else never°])
                  always°)
             (≈ #t q)
             => '(#t #t #t #t #t))

  ;; (check-run 5 (q)                   ; 38
  ;;            (all-i (cond-e [%s %s]
  ;;                           [else never°])
  ;;                   always°)
  ;;            (≈ #t q)
  ;;            => 'bad)
  )
