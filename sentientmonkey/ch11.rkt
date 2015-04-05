#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch04.rkt")

(provide two-in-a-row? two-in-a-row-b?)

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else (or (eq? a (car lat))
              (member? a (cdr lat)))]))

(test-case "member?"
  (check-true (member? 'sardines '(Italian sardines spaghetti parsley)))
  (check-false (member? 'sardines '(pizza))))

(define (equal-next? a lat)
  (cond
    [(null? lat) #f]
    [else (eq? a (car lat))]))

(check-equal? (equal-next? 'sardines '(sardines)) #t)
(check-equal? (equal-next? 'sardines '(parsley)) #f)
(check-equal? (equal-next? 'sardines '()) #f)

(define (two-in-a-row? lat)
  (cond
    [(null? lat) #f]
    [else
      (cond
        [(equal-next? (car lat) (cdr lat)) #t]
        [else
          (two-in-a-row? (cdr lat))])]))

(define (test-case-two-in-a-row? two-in-a-row?)
  (test-case "two-in-a-row?"
    (check-false (two-in-a-row? '(Italian sardines spaghetti parsley)))
    (check-true (two-in-a-row? '(Italian sardines sardines spaghetti parsley)))
    (check-false (two-in-a-row? '()))))

(test-case-two-in-a-row? two-in-a-row?)

(define (is-first-b? a lat)
  (cond
    [(null? lat) #f]
    [else (or (eq? a (car lat))
              (two-in-a-row-a? lat))]))

(define (two-in-a-row-a? lat)
  (cond
    [(null? lat) #f]
    [else
      (is-first-b? (car lat) (cdr lat))]))

(check-equal? (is-first-b? 'sardines '(sardines)) #t)
(check-equal? (is-first-b? 'sardines '(parsley)) #f)
(check-equal? (is-first-b? 'sardines '()) #f)

(test-case-two-in-a-row? two-in-a-row-a?)

(define (two-in-a-row-b? preceeding lat)
  (cond
    [(null? lat) #f]
    [else (or (eq? (car lat) preceeding)
              (two-in-a-row-b? (car lat) (cdr lat)))]))

(check-equal? (two-in-a-row-b? null '(Italian sardines spaghetti parsley)) #f)
(check-equal? (two-in-a-row-b? null '(Italian sardines sardines spaghetti parsley)) #t)

(define (two-in-a-row-final? lat)
  (cond
    [(null? lat) #f]
    [else (two-in-a-row-b? (car lat) (cdr lat))]))

(test-case-two-in-a-row? two-in-a-row-final?)

; first pass - ending up being backwards because of consing!
;
; (define (sum-of-prefixes tup)
;   (cond
;     [(null? tup) '()]
;     [else (sum-of-prefixes-b (list (car tup)) (cdr tup))]))
;
; (define (sum-of-prefixes-b sums tup)
;   (cond
;     [(null? tup) sums]
;     [else
;       (sum-of-prefixes-b
;         (cons (+ (car sums) (car tup)) sums)
;         (cdr tup))]))

(define (sum-of-prefixes tup)
  (sum-of-prefixes-b 0 tup))

(define (sum-of-prefixes-b ssof tup)
  (cond
    [(null? tup) '()]
    [else
      (cons (+ ssof (car tup))
            (sum-of-prefixes-b
              (+ ssof (car tup))
              (cdr tup)))]))

(check-equal? (sum-of-prefixes-b 0 '(1 1 1)) '(1 2 3))

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
(check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

; I remember pick from providing chapter 4, so ice cream?

(check-equal? (pick 4 '(4 3 1 1 1)) 1)
(check-equal? (pick 2 '(2 4 3 1 1 1)) 4)

(define (scramble-b tup rev-pre)
  (cond
    [(null? tup) '()]
    [else
      (cons (pick (car tup)
                  (cons (car tup) rev-pre))
                (scramble-b (cdr tup)
                            (cons (car tup) rev-pre)))]))

(define (scramble tup)
  (scramble-b tup '()))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
(check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1))
(check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2))

; I think I understand what scramble is, but I have no idea how it would be helpful for anything.
; I'm probably missing something important.

