#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch04.rkt")

(provide operator 1st-sub-exp 2nd-sub-exp)

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(check-equal? (numbered? '(3 + (4 x 5))) #t)
(check-equal? (numbered? '(3 + (4 ^ 5))) #t)
(check-equal? (numbered? '(2 x sausage)) #f)

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp)))))))))

(check-equal? (numbered2? '(3 + (4 x 5))) #t)
(check-equal? (numbered2? '(3 + (4 ^ 5))) #t)
(check-equal? (numbered2? '(2 x sausage)) #f)

(define value1
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (o+ (value1 (car nexp))
            (value1 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x)
       (x (value1 (car nexp))
            (value1 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '^)
       (pow (value1 (car nexp))
            (value1 (car (cdr (cdr nexp)))))))))

(check-equal? (value1 '13) 13)
(check-equal? (value1 '(1 + 3)) 4)
(check-equal? (value1 '(1 + (3 ^ 4))) 82)
(check-equal? (value1 '(3 + 4)) 7)

(define 1st-sub-exp1
  (lambda (aexp)
    (cond
      (else (car (cdr aexp))))))

(check-equal? (1st-sub-exp1 '(+ 3 4)) 3)

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(check-equal? (1st-sub-exp '(+ 3 4)) 3)

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(check-equal? (2nd-sub-exp '(+ 3 4)) 4)

(define operator
  (lambda (aexp)
    (car aexp)))

(check-equal? (operator '(+ 3 4)) '+)

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (o+ (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x)
       (x (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^)
       (pow (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp)))))))

(check-equal? (value2 '(+ 3 4)) 7)

(define 1st-sub-exp2
  (lambda (aexp)
    (car aexp)))

(check-equal? (1st-sub-exp2 '(3 + 4)) 3)

(define operator2
  (lambda (aexp)
    (car (cdr aexp))))

(check-equal? (operator2 '(3 + 4)) '+)

(define sero?
  (lambda (n)
    (null? n)))

(check-equal? (sero? '()) #t)

(define sadd1
  (lambda (n)
    (cons '() n)))

(check-equal? (sadd1 '()) '(()))

(define ssub1
  (lambda (n)
    (cdr n)))

(check-equal? (ssub1 '(())) '())

(define splus
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
        (sadd1 (splus n (ssub1 m)))))))

(check-equal? (splus '(() ()) '(() ())) '(() () () ()))

; from chapter 2
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(check-equal? (lat? '(1 2 3)) #t)
(check-equal? (lat? '(() () ())) #f)
