#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch03.rkt")
(require "ch04.rkt")
(require "ch07.rkt")

(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(number? sorn)
       (keep-looking a (pick sorn lat) lat)]
      [else
        (eq? sorn a)])))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(check-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
(check-false (looking 'caviar '(6 2 grits caviar 5 7 3)))

(check-equal? (pick 6 '(6 2 4 caviar 5 7 3 )) 7)
(check-equal? (pick 7 '(6 2 4 caviar 5 7 3 )) 3)

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(check-equal? (shift '((a b) c)) '(a (b c)))
(check-equal? (shift '((a b) (c d))) '(a (b (c d))))

(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (shift pora))]
      [else
        (build (first pora)
               (align (second pora)))])))

(define length*
  (lambda (pair)
    (cond
      [(atom? pair) 1]
      [else
        (o+ (length* (first pair))
            (length* (second pair)))])))

(check-equal? (length* '((a b) c)) 3)

(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else
        (o+ (x (weight* (first pora)) 2)
            (weight* (second pora)))])))

(check-equal? (weight* '((a b) c)) 7)
(check-equal? (weight* '(a (b c))) 5)

(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (shuffle (revpair pora))]
      [else
        (build (first pora)
               (shuffle (second pora)))])))

(check-equal? (shuffle '((a b) c)) '(c (a b)))
(check-equal? (shuffle '(a (b c))) '(a (b c)))
; never completes... (check-equal? (shuffle '((a b) (b c))) '((a b) (b c)))

(define C
  (lambda (n)
    (cond
      [(one? n) 1]
      [else
        (cond
          [(even? n) (C (div n 2))]
          [else
            (C (add1 (x 3 n)))])])))

(check-equal? (C 1) 1)
(check-equal? (C 2) 1)
(check-equal? (C 3) 1)
(check-equal? (C 4) 1)
(check-equal? (C 5) 1)

(define A
  (lambda (n m)
    (cond
      [(zero? n) (add1 m)]
      [(zero? m) (A (sub1 n) 1)]
      [else
        (A (sub1 n)
           (A n (sub1 m)))])))


(check-equal? (A 1 0) 2)
(check-equal? (A 1 1) 3)
(check-equal? (A 2 2) 7)
; no answer... (check-equal? (A 4 3) 0)

; re-written  mk-length
(check-equal? (((lambda (mk-length)
                (mk-length mk-length))
               (lambda (mk-length)
                 (lambda (l)
                   (cond
                     [(null? l) 0]
                     [else
                       (add1 ((mk-length eternity)
                              (cdr l)))]))))
               '(apples)) 1)

; le Y-combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
       (lambda (f)
         (le (lambda (x) ((f f) x)))))))

; Ceci n'est une factorial
(define factorial
  (Y (lambda (f)
       (lambda (n)
         (cond
           [(zero? n) 1]
           [else (x n (f (sub1 n)))])))))

(check-equal? (factorial 0) 1)
(check-equal? (factorial 1) 1)
(check-equal? (factorial 2) 2)
(check-equal? (factorial 3) 6)
(check-equal? (factorial 4) 24)
