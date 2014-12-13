#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; Previous chapters
(define o+
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (add1 (o+ x (sub1 y)))))))

(define ×
  (lambda (m n)
    (cond
     ((zero? n) 0)
     (else (o+ m (× m (sub1 n)))))))

(define ↑
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (× n (↑ n (sub1 m)))))))



(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp)  (number? aexp))
     ((eq? (cadr aexp) '+) (and (numbered? (car aexp)) (numbered? (caddr aexp))))
     ((eq? (cadr aexp) '↑) (and (numbered? (car aexp)) (numbered? (caddr aexp))))
     ((eq? (cadr aexp) '×) (and (numbered? (car aexp)) (numbered? (caddr aexp)))))))

(test-case "numbered?"
           (check-true  (numbered? 1))
           (check-true  (numbered? '(3 + (4 ↑ 5))))
           (check-false (numbered? '(2 × sausage))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (cadr nexp) '+) (o+ (value (car nexp)) (value (caddr nexp))))
     ((eq? (cadr nexp) '↑) (↑  (value (car nexp)) (value (caddr nexp))))
     ((eq? (cadr nexp) '×) (×  (value (car nexp)) (value (caddr nexp)))))))

(test-case "value"
           (check-eq? (value '13) 13)
           (check-eq? (value '(1 + 3)) 4)
           (check-eq? (value '(1 + (3 ↑ 4))) 82))

(define 1st-sub-exp2
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp2
  (lambda (aexp)
    (caddr aexp)))

(define operator2
  (lambda (aexp)
    (car aexp)))

(define value2
  (lambda (nexp)
    (cond
     ((and (atom? nexp) (number? nexp)) nexp)
     ((eq? (operator2 nexp) '+)
      (o+ (value (1st-sub-exp2 nexp)) (value (2nd-sub-exp2 nexp))))
     ((eq? (operator2 nexp) '↑)
      (↑ (value (1st-sub-exp2 nexp)) (value (2nd-sub-exp2 nexp))))
     ((eq? (operator2 nexp) '×)
      (× (value (1st-sub-exp2 nexp)) (value (2nd-sub-exp2 nexp)))))))


(test-case "value2"
           (check-eq? (value2 '13) 13)
           (check-eq? (value2 '(+ 1 3)) 4))


(define 1st-sub-exp3
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp3
  (lambda (aexp)
    (caddr aexp)))

(define operator3
  (lambda (aexp)
    (cadr aexp)))

(define value3
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator3 nexp) '+)
      (o+ (value (1st-sub-exp3 nexp)) (value (2nd-sub-exp3 nexp))))
     ((eq? (operator3 nexp) '↑)
      (↑ (value (1st-sub-exp3 nexp)) (value (2nd-sub-exp3 nexp))))
     ((eq? (operator3 nexp) '×)
      (× (value (1st-sub-exp3 nexp)) (value (2nd-sub-exp3 nexp)))))))


(test-case "value3"
           (check-eq? (value3 '13) 13)
           (check-eq? (value3 '(1 + 3)) 4)
           (check-eq? (value3 '(1 + (3 ↑ 4))) 82))


(define sero?
  (lambda (n)
    (null? n)))

(test-case "sero?"
           (check-true (sero? '()))
           (check-false (sero? '(()))))


(define edd1
  (lambda (n)
     (cons '() n)))

(test-case "edd1"
           (check-equal? (edd1 '()) '(()))
           (check-equal? (edd1 '(())) '(() ())))

(define zub1
  (lambda (n)
    (cdr n)))

(test-case "zub1"
           (check-equal? (zub1 '(() ())) '(()))
           (check-equal? (zub1 '(())) '()))


(define p+
  (lambda (x y)
    (cond
     ((sero? y) x)
     (else (edd1 (o+ x (zub1 y)))))))

