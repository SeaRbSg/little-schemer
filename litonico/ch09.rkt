#lang racket

(require "prelude.rkt")

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat) 
    (cond
      [(number? sorn) (keep-looking a (pick sorn lat) lat)]
      [else (eq? sorn a)])))

(define eternity
  (lambda (x)
    (eternity x)))


(define build 
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define first car)
(define second cadr)

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (shift pora))]
      [else (build (first pora)
                   (align (second pora)))])))

(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora))
               (length* (second pora)))])))

(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))])))

(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (revpair pora))]
      [else (build (first pora)
                   (shuffle (second pora)))])))

(define collatz ;; I reallllly hope this is solved in my lifetime
  (lambda (n)
    (cond
      [(one? n) 1]
      [(even? n) (collatz (/ n 2))]
      [else (collatz (add1 (* 3 n)))])))

(define A ;; Ackermann function- non-primitive-recursive
  (lambda (n m)
    (cond
      [(zero? n) (+ 1 m)]
      [(zero? m) (A (- n 1))]
      [else (A (- n 1)
               (A n (- m 1)))])))
