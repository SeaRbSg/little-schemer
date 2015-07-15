#lang racket

(require rackunit)


(define-syntax var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var?
  (syntax-rules ()
    ((_ x) (vector? x))))

(define-syntax run
  (syntax-rules ()
    ((_ num-results (x) g ...)
     (let [(n num-results) (x (var x)
