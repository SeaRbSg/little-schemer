#lang racket

(provide atom?)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(provide eqan?) ;; Universal equality

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2) (equal? a1 a2))] ; numeric equality
      [(or (number? a1) (number? a2) #f)]
      [else (eq? a1 a2)])))

(provide pick)

(define pick ;; 1-based indexing, just because
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))
