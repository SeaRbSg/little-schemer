#lang racket
(require rackunit)

;; refactoring a couple of methods from last chapter
;; to use letrec and hide their utility functions.
(define two-in-a-row?
  (lambda (lat)
    (letrec ([T (lambda (preceding lat)
                  (cond
                   [(null? lat) #f]
                   [else 
                    (or (eq? preceding (car lat))
                        (T (car lat) (cdr lat)))]))])
      (cond
       [(null? lat) #f]
       [else (T (car lat) (cdr lat))]))))

(check-true (two-in-a-row? '(1 1)))
(check-true (two-in-a-row? '(1 2 3 3 4)))
(check-false (two-in-a-row? '(1 2 3 4 5)))
(check-false (two-in-a-row? '()))

(define sum-of-prefixes
  (lambda (tup)
    (letrec ([S (lambda (sum tup)
                  (cond
                   [(null? tup) '()]
                   [else
                    (cons (+ sum (car tup))
                          (S (+ sum (car tup))
                             (cdr tup)))]) )])
      (S 0 tup))))

(check-equal? (sum-of-prefixes '(2 1 9 17 0))
              '(2 3 12 29 29))

(check-equal? (sum-of-prefixes '(1 1 1 1 1))
              '(1 2 3 4 5))
