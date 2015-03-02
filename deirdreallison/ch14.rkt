#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (let/cc success 
       (let/cc var (success a)) . b))))

(define pick
  (lambda (n lat)
    (cond
      [(eq? n '1) (car lat)]
      [else (pick (- n 1) (cdr lat))])))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (cond
              ((atom? (leftmost (car l)))
               (leftmost (car l)))
              (else (leftmost (cdr l))))))))





(displayln (leftmost '(((a) b) (cd))))
(displayln (leftmost '(((() a) ()))))
(displayln (leftmost '(((a) ()) () (e))))