#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (let/cc success 
       (let/cc var (success a)) . b))))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else 
       (let 
           ((a leftmost (car l))))
       (cond
         ((atom? a) a)
         (else (leftmost (cdr l))))))))





(displayln (leftmost '(((a) b) (cd))))
(displayln (leftmost '(((() a) ()))))
(displayln (leftmost '(((a) ()) () (e))))