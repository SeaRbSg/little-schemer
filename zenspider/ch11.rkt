#lang racket/base

(require "lib/shared.rkt")

(module+ test
  (require rackunit))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond [(null? lat) #f]
          [else (or (eq? (car lat) preceding)
                    (two-in-a-row-b? (car lat) (cdr lat)))])))

(define two-in-a-row?
  (lambda (lat)
    (cond [(null? lat) #f]
          [else
           (two-in-a-row-b? (car lat) (cdr lat))])))

(module+ test
  (test-case "two-in-a-row?"
    (check-false (two-in-a-row? '(a b c d)))
    (check-false (two-in-a-row? '(a b a d)))
    (check-true (two-in-a-row? '(a b b d)))
    (check-true (two-in-a-row? '(a a c d)))
    (check-true (two-in-a-row? '(a b c c)))))

(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond [(null? tup) '()]
          [else (cons (+ sum (car tup))
                      (sum-of-prefixes-b (+ sum (car tup)) (cdr tup)))])))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(module+ test
  (test-case "sum-of-prefixes"
    (check-equal? (sum-of-prefixes '(1 1 1 1 1))
                  '(1 2 3 4 5))
    (check-equal? (sum-of-prefixes '(2 1 9 17 0))
                  '(2 3 12 29 29))))

(define pick
  (lambda (n lat)
    (cond [(= 1 n) (car lat)]
          [else (pick (sub1 n) (cdr lat))])))

(define scramble-b
  (lambda (tup rev-pre)
    (cond [(null? tup) '()]
          [else
           (cons (pick (car tup) (cons (car tup)
                                       rev-pre))
                 (scramble-b (cdr tup) (cons (car tup)
                                             rev-pre)))])))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(module+ test
  (test-case "scramble"
    (check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
                  '(1 1 1 1 1 4 1 1 1 9))
    (check-equal? (scramble '(1 2 3 4 5 6 7 8 9))
                  '(1 1 1 1 1 1 1 1 1))))
