#lang racket

(require (rename-in racket (car s.car)
                           (cdr s.cdr)
                           (+ s.+)
                           (< s.<)
                           (if s.if)))

(provide (all-defined-out))

(define (num x) (s.if (number? x) x 0))
(define (if/nil Q A E)
  (s.if (equal? Q 'nil) (E) (A)))

(define (atom x) (s.if (pair? x) 'nil 't))
(define (car x) (s.if (pair? x) (s.car x) '()))
(define (cdr x) (s.if (pair? x) (s.cdr x) '()))
(define (equal x y) (s.if (equal? x y) 't 'nil))
(define (natp x)
  (s.if (integer? x) (s.if (< x 0) 'nil 't) 'nil))
(define (+ x y) (s.+ (num x) (num y)))
(define (< x y)
  (s.if (s.< (num x) (num y)) 't 'nil))

(define-syntax if
  (syntax-rules ()
    ((_ Q A E)
     (if/nil Q (lambda () A) (lambda () E)))))

(define-syntax defun
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(defun size (x)
  (if (atom x)
    '0
    (+ '1 (size (car x)) (size (cdr x)))))
