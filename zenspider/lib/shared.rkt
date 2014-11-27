#lang racket/base

(provide atom?
         test)

(require rackunit)
(require (for-syntax racket/base))
(define-syntax test (make-rename-transformer #'check-equal?)) ; TODO: deprecate

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
