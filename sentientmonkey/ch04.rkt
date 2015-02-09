#lang racket/base

(require rackunit)
(require "prelude.rkt")

(provide o+ o- x div pow gt lt eq eqan? pick one?)

(check-true (atom? 14))
(check-true (number? -3))
(check-true (number? 3.14159))

(define add1
  (lambda (n)
    (+ n 1)))

(check-equal? (add1 67) 68)

(define sub1
  (lambda (n)
    (- n 1)))

(check-equal? (sub1 5) 4)
(check-equal? (sub1 0) -1)

(check-true (zero? 0))
(check-false (zero? 1492))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(check-equal? (o+ 46 12) 58)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(check-equal? (o- 14 3) 11)
(check-equal? (o- 17 9) 8)
(check-equal? (o- 18 25) -7)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)) )))))

(check-equal? (addtup '(1 2 3 4 5)) 15)

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(check-equal? (x 5 3) 15)
(check-equal? (x 13 4) 52)
(check-equal? (x 12 3) 36)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
              (tup+
                (cdr tup1) (cdr tup2)))))))

(check-equal? (tup+ '(3 7 8 1) '(4 6)) '(7 13 8 1))

; > implementation
(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (gt (sub1 n) (sub1 m))))))

(check-false (gt 12 133))
(check-true (gt 120 11))

(check-false (gt 3 3))

; < definition
(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (lt (sub1 n) (sub1 m))))))

(check-true (lt 4 6))
(check-false (lt 8 3))
(check-false (lt 6 6))

(define eq
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
      (else #t))))

(check-true (eq 6 6))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
        (x n (pow n (sub1 m)))))))

(check-equal? (pow 1 1) 1)
(check-equal? (pow 2 3) 8)
(check-equal? (pow 5 3) 125)

; division
(define div
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (div (o- n m) m))))))

(check-equal? (div 15 4) 3)

; length
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(check-equal? (len '(hotdogs with mustard sauerkraut and pickles)) 6)
(check-equal? (len '(hame and cheese on rye)) 5)

; pick
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(check-equal? (pick 4 '(lasanga spaghetti ravioli macaroni meatball)) 'macaroni)

(check-false (number? 'tomato))
(check-true (number? 76))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (no-nums (cdr lat)))
      (else
        (cons (car lat)
              (no-nums (cdr lat)))))))

(check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (eq a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(check-true (eqan? 1 1))
(check-false (eqan? 1 'tomato))
(check-true (eqan? 'tomato 'tomato))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a)
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(check-equal? (occur 'apples '(apples and apples and oranges)) 2)

; defintion of one (used simple version on first attempt)
(define one?
  (lambda (n)
    (eq n 1)))

(check-true (one? 1))
(check-false (one? 42))

; rempick refactored
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n)
                       (cdr lat)))))))

(check-equal? (rempick 3 '(lemon meringue salty pie)) '(lemon meringue pie))
