#lang racket

(provide %u %s)

(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define %s
  (lambda (s)
    (unit s)))

(define %u
  (lambda (s)
    (mzero)))
