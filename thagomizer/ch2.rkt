#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; Lat?
(define lat? 
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(test-case "Lat?"
           (check-true (lat? '(Jack Sprat could eat no chicken fat)))
           (check-true (lat? '()))
           (check-false (lat? '((Jack) Sprat could eat no chicken fat)))
           (check-false (lat? '(Jack (Sprat could) eat no chicken fat)))
           )

;; Member?
(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(test-case "member?"
           (check-true (member? 'tea '(coffee tea or milk)))
           (check-false (member? 'poached '(fried eggs and scrambled eggs)))
           )

