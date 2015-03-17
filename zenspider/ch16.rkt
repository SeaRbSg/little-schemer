;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit))

(require "lib/shared.rkt")

(define (find n Ns Rs)
  (letrec ([A (lambda (ns rs)
                (cond [(= (car ns) n) (car rs)]
                      [else (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else (cons (deepM (sub1 m))
                    '())]))

(define deepM
  (let ([Ns '()]
        [Rs '()])
    (lambda (n)
      (if (member n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(module+ test
  (check-equal? (deepM 5)
                '(((((pizza))))))
  (check-equal? (deepM 3)
                '(((pizza)))))

;; pg 119:

(define (length l)
  (cond [(null? l) 0]
        [else (add1 (length (cdr l)))]))

(set! length (lambda (l) 0))

(set! length
      (lambda (l)
        (cond [(null? l) 0]
              [else (add1 (length (cdr l)))])))

(set! length
      (let ((h (lambda (l) 0)))
        (set! h
              (lambda (l)
                (cond [(null? l) 0]
                      [else (add1 (h (cdr l)))])))
        h))

;; pg 122

(define L
  (lambda (length)
    (lambda (l)
      (cond [(null? l) 0]
            [else (add1 (length (cdr l)))]))))

(set! length
      (let ([h (lambda (l) 0)])
        (set! h (L (lambda (arg) (h arg))))
        h))

(module+ test
  (check-equal? (length '(1 2 3))
                3))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(set! length (Y! L))
(set! length (Y-bang L))

(module+ test
  (check-equal? (length '(1 2 3))
                3))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond [(null? s) 1]
            [(atom? (car s)) (depth* (cdr s))]
            [else (max (add1 (depth* (car s)))
                       (depth* (cdr s)))]))))

(define depth* (Y! D))

(module+ test
  (check-equal? (depth* '(a (b (c))))
                3))
