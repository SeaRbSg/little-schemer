#lang racket
(require rackunit)

;; The applicative-order imperative Y combinator.
;; It produces recursive definitions without requiring
;; that thefunctions be named by (define ...)
(define Y!
  (lambda (f)
    (letrec
        [(h (f (lambda (arg) (h arg))))]
      h)))

(define L
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

(define length (Y! L))

(check-equal? (length '(1 2 3))
              3)
