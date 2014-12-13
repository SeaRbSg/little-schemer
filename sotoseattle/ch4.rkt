#lang racket

(require "lib/shared.rkt")
(require rackunit)

(module+ test
  (check-equal? (add1 3) 4)
  (check-equal? (sub1 3) 2)
  (check-equal? (zero? 0) #t)
  (check-equal? (+ 1 3) 4))


; ###### WRINTING FUNCTIONS!!!!! ##########

(define ✢ ; <-------------------------------- ADD
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (✢ n (sub1 m)))))))

(module+ test
  (check-equal? (✢ 1 3) 4))

(define ━ ; <-------------------------------- SUBSTRACT
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (━ n (sub1 m)))))))

(module+ test
  (check-equal? (━ 3 1) 2)
  (check-equal? (━ 30 7) 23))

(define addtup ; <-------------------------------- ADD_TUPPLE
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (✢ (car tup) (addtup (cdr tup)))))))

(module+ test
  (check-equal? (addtup '(1 2 3)) 6))

(define x ; <-------------------------------- MUTIPLY
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (✢ n (x n (sub1 m)))))))

(module+ test
  (check-equal? (x 2 5) 10))

(define tup+ ; <-------------------------------- ADD_ELEMENTS_IN_TUPPLE
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else
       (cons (✢ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(module+ test
  (check-equal? (tup+ '(1 2 3) '(3 2 1)) '(4 4 4)))

(define tupbetter+ ; <-------------------------------- ADD_ELEMENTS_IN_TUPPLE_IMPROVED
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (✢ (car tup1) (car tup2)) (tupbetter+ (cdr tup1) (cdr tup2)))))))

(module+ test
  (check-equal? (tupbetter+ '(1 2 3) '(3 2)) '(4 4 3)))

(define > ; <-------------------------------- IS_GREATER?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(module+ test
  (check-true (> 3 2))
  (check-false (> 3 20))
  (check-false (> 3 3)))

(define < ; <-------------------------------- IS_SMALLER?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(module+ test
  (check-false (< 3 2))
  (check-true (< 3 20))
  (check-false (< 3 3)))

(define o= ; <-------------------------------- IS_EQUAL? (MINE)
  (lambda (n m)
    (and (eq? (> n m) #f) (eq? (< n m) #f))))

(define = ; <--------------------------------- IS_EQUAL? (BOOK)
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(module+ test
  (check-false (o= 3 2))
  (check-true (o= 3 3))
  (check-false (=  3 2))
  (check-true (=  3 3)))

(define ** ; <-------------------------------- EXPONENTIATION
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (** n (sub1 m)))))))

(module+ test
  (check-equal? (** 2 3) 8)
  (check-equal? (** 3 2) 9))

(define // ; <-------------------------------- DIVISION
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (// (━ n m) m))))))
      ;(else (sub1 (━ n (sub1 m))))

(module+ test
  (check-equal? (// 15 3) 5)
  (check-equal? (// 15 4) 3))

(define length ; <-------------------------------- LENGTH
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(module+ test
  (check-equal? (length '(hotdogs with mustard sauerkraut and pickles)) 6))

(define pick ; <-------------------------------- []
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(module+ test
  (check-equal? (pick 1 '(10 20 30)) 10)
  (check-equal? (pick 3 '(10 20 30)) 30))

(define rempick ; <-------------------------------- DELETE_AT
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(module+ test
  (check-equal? (rempick 1 '(10 20 30)) '(20 30))
  (check-equal? (rempick 3 '(10 20 30)) '(10 20))
  (check-true (number? 76))
  (check-false (number? 'pepe)))

(define no-nums ; <-------------------------------- REMOVE_NUMBERS
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))

(module+ test
  (check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates)))

(define all-nums ; <-------------------------------- EXTRACT_NUMBERS
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(module+ test
  (check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9)))

(define eqan? ; <-------------------------------- SAME_ATOM?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(module+ test
  (check-true (eqan? 2 2))
  (check-false (eqan? 2 3))
  (check-true (eqan? 'pepe 'pepe))
  (check-false (eqan? 'pepe 'juan))
  (check-false (eqan? 2 'pepe))
  (check-true (eqan? 2 '2)))
; (atom? 2) == (atom? '2) == (num? '2) == (num? 2) == true

(define occur ; <-------------------------------- OCCURRENCES
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(module+ test
  (check-equal? (occur 1 '(2 3 1 1 4 1)) 3)
  (check-equal? (occur 7 '(2 3 1 1 4 1)) 0))

(define one? ; <-------------------------------- IS_ONE?
  (lambda (n)
    (eqan? n 1))) ; <============================= WHY NOT THIS WAY?

(module+ test
  (check-true (one? 1))
  (check-false (one? 2))
  (check-false (one? 'pepe)))

(define rempick2 ; <-------------------------------- DELETE_AT (revised)
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

(module+ test
  (check-equal? (rempick2 1 '(10 20 30)) '(20 30))
  (check-equal? (rempick2 3 '(10 20 30)) '(10 20)))
