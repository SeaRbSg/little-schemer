#lang racket/base

(provide operator 1st-sub-exp 2nd-sub-exp)

(require "lib/shared.rkt")
(require "ch04.rkt")                    ; eqan? ** pick div

(module+ test
  (require rackunit))

;;; Chapter 6
;; pg 97 - 99

(define numbered1?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [(eq? (car (cdr aexp)) '+) #t]
     [(eq? (car (cdr aexp)) '*) #t]
     [(eq? (car (cdr aexp)) '^) #t]
     [else #f])))

(module+ test
  (define (test/numbered numbered?)
    (check-true  (numbered? 42))
    (check-false (numbered? 'a))
    (check-true  (numbered? '(1 + 1)))
    (check-true  (numbered? '(1 * 1)))
    (check-false (numbered? '(1 / 1)))
    (check-true (numbered? '((1 + 1) * (1 * 1))))
    (check-true  (numbered? '(1 ^ 1))))
  (test/numbered numbered1?))

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
           (numbered2 (car (cdr (cdr aexp)))))]
     [else #f])))

(module+ test
  (test/numbered numbered2))

;; lame version - doesn't ask about op
(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [else
      (and (memq (cadr aexp) '(+ * ^))
           (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))])))

(module+ test
  (test/numbered numbered?))

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
      (expt (value1 (car exp))
            (value1 (car (cdr (cdr exp)))))])))

(module+ test
  (define (test/value/infix value)
    (check-equal? (value '(1 + 3))        4)
    (check-equal? (value '(1 + (3 * 4))) 13)
    (check-equal? (value '(1 + (2 ^ 3)))  9))

  (test/value/infix value1))

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
      (expt (value2 (car (cdr exp)))
            (value2 (car (cdr (cdr exp)))))])))

(module+ test
  (define (test/value/pre value)
    (check-equal? (value '(+ 3 1))        4)
    (check-equal? (value '(+ 1 (* 3 4))) 13)
    (check-equal? (value '(+ 1 (^ 2 3)))  9))

  (test/value/pre value2))

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
      (expt (value3 (1st-sub-exp exp))
            (value3 (2nd-sub-exp exp)))])))

(module+ test
  (test/value/pre value3))

;; pg 107

(define sero?
  (lambda (n)
    (null? n)))

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

(module+ test
  (check-true (sero? '()))
  (check-false (sero? 4))

  (check-equal? '(()) (edd1 '()))
  (check-equal? '()   (zub1 (edd1 '())))

  (check-equal? (pluz '() '())       '(    ))
  (check-equal? (pluz '(()) '())     '((  )))
  (check-equal? (pluz '() '(()))     '((  )))
  (check-equal? (pluz '(()) '())     '((  ))))
