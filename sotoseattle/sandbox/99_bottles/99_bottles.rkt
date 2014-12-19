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

;;; segments of verses
(define bottles_of_beer
  (lambda (n)
    (cond
      ((eq? n 1) (cons n '(bottle of beer)))
      ((zero? n) '(no more bottles of beer))
      (else (cons n '(bottles of beer))))))

(define bottles_on_wall
  (lambda (n)
    (cons (bottles_of_beer n) '(on the wall))))

;;; verses as lines in the song, grouped in chorus
(define verse_1
  (lambda (n)
    (cons (bottles_on_wall n) (bottles_of_beer n))))

(define verse_2
  (lambda (n)
    (cond
      ((eq? n -1) '(Go to the store and buy some more!!!!!!))
      (else
        (cons '(Take one down and pass it around) (bottles_on_wall n))))))

(define chorus
  (lambda (n)
    (cons (verse_1 n) (verse_2 (sub1 n)))))

;;; al together now!!
(define sing
  (lambda (lan)
    (cond
      ((null? lan) lan)
      (else
        (cons (chorus(car lan)) (sing (cdr lan)))))))

(sing (countdown 3)) ;; Bravo!!