#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


(test-case "pages 59 & 60"
           (check-true (atom? 14))
           (check-true (number? -3))
           (check-true (number? 3.14159))
           (check-eq? (add1 67) 68)
           (check-eq? (sub1 5) 4)
           (check-true (zero? 0))
           (check-false (zero? 1492)))


;; This was my first attempt
;; (define o+ 
;;   (lambda (x y)
;;     (cond
;;      ((zero? x) y)
;;      (else (o+ (sub1 x) (add1 y))))))

;; This is the book's definition. Both work.
(define o+
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (add1 (o+ x (sub1 y)))))))

(test-case "o+"
           (check-eq? (o+ 46 12) 58))


(define o-
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (sub1 (o- x (sub1 y)))))))

(test-case "o-"
           (check-eq? (o- 14 3) 11)
           (check-eq? (o- 17 9) 8)
           (check-eq? (o- 18 25) -7))


(define tup?
  (lambda (tup)
    (cond
     ((null? tup) #t)
     ((number? (car tup)) (tup? (cdr tup)))
     (else #f))))

(test-case "tup?"
           (check-true (tup? '())) 
           (check-true (tup? '(2 11 3 79 47 6)))
           (check-true (tup? '(8 55 5 555)))
           (check-false (tup? '(1 2 8 apple 4 3)))
           (check-false (tup? '(3 (7 4) 13 9))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(test-case "addtup"
           (check-eq? (addtup '(3 5 2 8)) 18)
           (check-eq? (addtup '(15 6 7 12 3)) 43))


(define ×
  (lambda (m n)
    (cond
     ((zero? n) 0)
     (else (o+ m (× m (sub1 n)))))))

(test-case "×"
           (check-eq? (× 5 3) 15)
           (check-eq? (× 13 4) 52))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(test-case "tup+"
           (check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
           (check-equal? (tup+ '(2 3) '(4 6)) '(6 9))
           (check-equal? (tup+ '(3 7) '(4 6)) '(7 13)))
