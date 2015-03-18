;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit))

(require "lib/shared.rkt")

(define (find n Ns Rs)                  ; pg 113
  (letrec ([A (lambda (ns rs)
                (cond [(= (car ns) n) (car rs)]
                      [else (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

(define (deep m)                        ; pg 115
  (cond [(zero? m) 'pizza]
        [else (cons (deepM (sub1 m))
                    '())]))

(define deepM                           ; pg 116
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

(module+ test
  (define (test/length length)
    (check-equal? (length '(1 2 3))
                  3))

  (define (length l)                    ; pg 118
    (cond [(null? l) 0]
          [else (add1 (length (cdr l)))]))

  (test/length length)

  (set! length (lambda (l) 0))          ; pg 119

  (check-equal? (length '(1 2 3)) 0)    ; explicitly wrong

  (set! length
        (lambda (l)
          (cond [(null? l) 0]
                [else (add1 (length (cdr l)))])))

  (test/length length)

  (set! length
        (let ((h (lambda (l) 0)))
          (set! h
                (lambda (l)
                  (cond [(null? l) 0]
                        [else (add1 (h (cdr l)))])))
          h))

  (test/length length))

;; pg 122

(define L
  (lambda (length)
    (lambda (l)
      (cond [(null? l) 0]
            [else (add1 (length (cdr l)))]))))

(module+ test
  (set! length
        (let ([h (lambda (l) 0)])
          (set! h (L (lambda (arg) (h arg))))
          h)))

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

(module+ test
  (define (test/y-length length)
    (check-equal? (length '(1 2 3))
                  3))
  (test/y-length (Y! L))
  (test/y-length (Y-bang L)))

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
