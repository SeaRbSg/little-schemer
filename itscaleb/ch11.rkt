#lang racket
(require rackunit)

(define two-in-a-row?
  (lambda (lat)
    (cond
     [(null? lat) #f]
     [else
      (two-in-a-row (car lat) (cdr lat))])))

;; two-in-a-row adds an additional
;; argument to know what the previous element in the list was.
(define two-in-a-row
  (lambda (preceding lat)
    (cond
     [(null? lat) #f]
     [else
      (or (eq? preceding (car lat))
          (two-in-a-row (car lat) (cdr lat)))])))

(check-true (two-in-a-row? '(1 1)))
(check-true (two-in-a-row? '(1 2 3 3 4)))
(check-false (two-in-a-row? '(1 2 3 4 5)))
(check-false (two-in-a-row? '()))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

;; sum of prefixes uses an additional argument to store
;; the running sum of the previous numbers in the tuple
(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond
     [(null? tup) '()]
     [else
      (cons (+ sum (car tup))
            (sum-of-prefixes-b (+ sum (car tup))
                               (cdr tup)))])))

(check-equal? (sum-of-prefixes '(2 1 9 17 0))
              '(2 3 12 29 29))

(check-equal? (sum-of-prefixes '(1 1 1 1 1))
              '(1 2 3 4 5))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

;; scramble adds an additional argument to have access to all
;; the previous numbers in the tuple
(define scramble-b
  (lambda (tup rev-pre)
    (cond
     [(null? tup) '()]
     [else
      (let* [(index (car tup))
             (next-pre (cons index rev-pre))]
        (cons (pick index next-pre)
              (scramble-b (cdr tup) next-pre)))])))

(define pick
  (lambda (n lat)
    (cond
     [(= 1 n) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
              '(1 1 1 1 1 4 1 1 1 9))

(check-equal? (scramble '(1 2 3 4 5 6 7 8 9))
              '(1 1 1 1 1 1 1 1 1))

(check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10))
              '(1 1 1 1 1 1 1 1 2 8 2))

;; all of these examples lead up to the eleventh commandment:
;; "Use additional arguments when a function needs to know what
;; other arguments to the function have been like so far"

;; the pattern seems to be to create a helper function that provides
;; a simple interface, while a second function does the real work of
;; recursing and also keeping track of previous arguments to the function.


