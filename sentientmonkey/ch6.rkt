#lang racket
(require test-engine/racket-tests)

; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; chapter 2
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
        (x n (pow n (sub1 m)))))))

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

(check-expect (numbered? '(3 + (4 x 5))) #t)
(check-expect (numbered? '(3 + (4 ^ 5))) #t)
(check-expect (numbered? '(2 x sausage)) #f)

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp)))))))))

(check-expect (numbered2? '(3 + (4 x 5))) #t)
(check-expect (numbered2? '(3 + (4 ^ 5))) #t)
(check-expect (numbered2? '(2 x sausage)) #f)

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

(check-expect (value1 '13) 13)
(check-expect (value1 '(1 + 3)) 4)
(check-expect (value1 '(1 + (3 ^ 4))) 82)
(check-expect (value1 '(3 + 4)) 7)

(define 1st-sub-exp1
  (lambda (aexp)
    (cond
      (else (car (cdr aexp))))))

(check-expect (1st-sub-exp1 '(+ 3 4)) 3)

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(check-expect (1st-sub-exp '(+ 3 4)) 3)

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(check-expect (2nd-sub-exp '(+ 3 4)) 4)

(define operator
  (lambda (aexp)
    (car aexp)))

(check-expect (operator '(+ 3 4)) '+)

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

(check-expect (value2 '(+ 3 4)) 7)

(define 1st-sub-exp2
  (lambda (aexp)
    (car aexp)))

(check-expect (1st-sub-exp2 '(3 + 4)) 3)

(define operator2
  (lambda (aexp)
    (car (cdr aexp))))

(check-expect (operator2 '(3 + 4)) '+)

(define sero?
  (lambda (n)
    (null? n)))

(check-expect (sero? '()) #t)

(define sadd1
  (lambda (n)
    (cons '() n)))

(check-expect (sadd1 '()) '(()))

(define ssub1
  (lambda (n)
    (cdr n)))

(check-expect (ssub1 '(())) '())

(define splus
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
        (sadd1 (splus n (ssub1 m)))))))

(check-expect (splus '(() ()) '(() ())) '(() () () ()))

; from chapter 2
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(check-expect (lat? '(1 2 3)) #t)
(check-expect (lat? '(() () ())) #f)

(test)
