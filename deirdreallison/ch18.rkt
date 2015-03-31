#lang racket

(define lots
  (lambda (m)
    (cond
      ((zero? m) (quote ()))
      (else (cons (quote egg)
                  (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (cdr l)))))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(displayln (lots 3))
(displayln (lenkth '(lots 3)))