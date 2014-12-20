#lang racket

(require "prelude.rkt")
(require rackunit)

(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [(eq? (cadr aexp) '+)
       (and (numbered? (car aexp)) 
            (numbered? (caddr aexp)))]
      [(eq? (cadr aexp) '*)
       (and (numbered? (car aexp)) 
            (numbered? (caddr aexp)))]
      [(eq? (cadr aexp) '^)
       (and (numbered? (car aexp)) 
            (numbered? (caddr aexp)))]
      [else #f])))

;; Numbered1? accepts any symbol as an infix operator, 
;; rather than just '+, '*', and '^
(define numbered1?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp))
                 (numbered? (caddr aexp)))])))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (cadr nexp) '+)
       (+ (value (car nexp)) 
            (value (caddr nexp)))]
      [(eq? (cadr nexp) '*)
       (* (value (car nexp)) 
            (value (caddr nexp)))]
      [else (expt (value (car nexp)) 
            (value (caddr nexp)))])))

(define prefix-value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car nexp) '+)
       (+ (prefix-value (cdr nexp)) 
            (prefix-value (cddr nexp)))]
      [(eq? (car nexp) '*)
       (* (prefix-value (cdr nexp)) 
            (prefix-value (cddr nexp)))]
      [else (expt (prefix-value (cdr nexp)) 
            (prefix-value (cddr nexp)))])))

(define fst-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define snd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

;; caddr & family make the next one the same as above

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define edd
  (lambda (x y)
    (cond
      [(sero? y) x]
      [else (edd (edd1 x) (zub1 y))])))

;; Study-session updated methods
(define op?
  (lambda (x)
    (cond 
      [(eq? x '+) #t]
      [(eq? x '*) #t]
      [(eq? x '^) #t])))

(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (op? (caddr aexp))])))
