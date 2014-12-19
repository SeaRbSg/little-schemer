#lang racket

(require "../../lib/shared.rkt")
(require rackunit)

(define countdown
  (lambda (n)
    (cond
      ((zero? n) '(0))
      (else (cons n (countdown (sub1 n)))))))

(module+ test
  (check-equal? (countdown 3) '(3 2 1 0)))

(define concat
  (lambda (lat1 lat2)
    (cond 
      ((null? lat1) lat2)
      (else (cons (car lat1) (concat (cdr lat1) lat2))))))

;;; segments of verses
(define bottles_of_beer
  (lambda (n)
    (cond
      ((zero? n) '(no more bottles of beer))
      ((eq? n 1) (cons n '(bottle of beer)))
      (else (cons n '(bottles of beer))))))

(define on_wall
  (lambda (n)
    (concat (bottles_of_beer n) '(on the wall *))))

;;; verses as lines in the song, grouped in chorus
(define verse_1
  (lambda (n)
    (concat (on_wall n) (bottles_of_beer n))))

(define verse_2
  (lambda (n)
    (cond
      ((eq? n -1) '(Go to the store and buy some more!!!!!!))
      (else (concat '(* Take one down and pass it around *) (on_wall n))))))

(define chorus
  (lambda (n)
    (concat (verse_1 n) (verse_2 (sub1 n)))))

(define sing
  (lambda (n)
    (cond
      ((eq? n -1) '())
      (else (cons (chorus n) (sing (sub1 n)))))))

(sing 99) ;; Bravo!!
