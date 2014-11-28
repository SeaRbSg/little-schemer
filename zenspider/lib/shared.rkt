#lang racket/base

(provide atom?)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
