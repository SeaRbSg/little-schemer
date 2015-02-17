#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f )
      (else
       (or (is-first? (car lat) (cdr lat))
           (two-in-a-row? (cdr lat)))))))