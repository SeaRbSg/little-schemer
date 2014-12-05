#lang racket/base

(provide operator 1st-sub-exp 2nd-sub-exp)

(require rackunit)
(require "lib/shared.rkt")
(require "ch04.rkt")                    ; eqan? ** pick div

;;; Chapter 6
;; pg 97 - 99

(define numbered1?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [(eq? (car (cdr aexp)) '+) #t]
     [(eq? (car (cdr aexp)) '*) #t]
     [(eq? (car (cdr aexp)) '^) #t])))

;; pg 100 - 101

(define numbered2
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [(eq? (car (cdr aexp)) '+)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) '*)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp)))))]
     [(eq? (car (cdr aexp)) '^)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp)))))])))

;; lame version - doesn't ask about op
(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))])))

;; pg 102 - 103
(define value1
  (lambda (exp)
    (cond
     [(atom? exp) exp]
     [(eq? (car (cdr exp)) '+)
      (+ (value1 (car exp))
         (value1 (car (cdr (cdr exp)))))]
     [(eq? (car (cdr exp)) '*)
      (* (value1 (car exp))
         (value1 (car (cdr (cdr exp)))))]
     [(eq? (car (cdr exp)) '^)
      (** (value1 (car exp))
          (value1 (car (cdr (cdr exp)))))])))

(check-equal? (eq? 4 (value1 '(1 + 3)))
              #t)
(check-equal? (eq? 13 (value1 '(1 + (3 * 4))))
              #t)

;; pg 104 - 105

(define value2
  (lambda (exp)
    (cond
     [(atom? exp) exp]
     [(eq? (car exp) '+)
      (+ (value2 (car (cdr exp)))
         (value2 (car (cdr (cdr exp)))))]
     [(eq? (car exp) '*)
      (* (value2 (car (cdr exp)))
         (value2 (car (cdr (cdr exp)))))]
     [(eq? (car exp) '^)
      (** (value2 (car (cdr exp)))
          (value2 (car (cdr (cdr exp)))))])))

(check-equal? (eq? 4 (value2 '(+ 1 3)))
              #t)
(check-equal? (eq? 13 (value2 '(+ 1 (* 3 4))))
              #t)

(define 1st-sub-exp
  (lambda (exp)
    (car (cdr exp))))

;; pg 106

(define 2nd-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
  (lambda (exp)
    (car exp)))

(define value3
  (lambda (exp)
    (cond
     [(atom? exp) exp]
     [(eq? (operator exp) '+)
      (+ (value3 (1st-sub-exp exp))
         (value3 (2nd-sub-exp exp)))]
     [(eq? (operator exp) '*)
      (* (value3 (1st-sub-exp exp))
         (value3 (2nd-sub-exp exp)))]
     [(eq? (operator exp) '^)
      (** (value3 (1st-sub-exp exp))
          (value3 (2nd-sub-exp exp)))])))

(check-equal? (eq? 4 (value3 '(+ 1 3)))
              #t)
(check-equal? (eq? 13 (value3 '(+ 1 (* 3 4))))
              #t)

;; pg 107

(define sero?
  (lambda (n)
    (null? n)))

(check-equal? (sero? '())
              #t)
(check-equal? (sero? 4)
              #f)

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define pluz
  (lambda (n m)
    (cond
     [(sero? m) n]
     [else
      (edd1 (pluz n (zub1 m)))])))
