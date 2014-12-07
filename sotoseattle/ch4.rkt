#lang racket
(require test-engine/racket-tests)

; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))
(check-expect (atom? (quote())) #f)
(check-expect (add1 3) 4)
(check-expect (sub1 3) 2)
(check-expect (zero? 0) #t)
(check-expect (+ 1 3) 4)


; ###### WRINTING FUNCTIONS!!!!! ##########

(define ✢ ; <-------------------------------- ADD
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (✢ n (sub1 m)))))))

(check-expect (✢ 1 3) 4)

(define ━ ; <-------------------------------- SUBSTRACT
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (━ n (sub1 m)))))))

(check-expect (━ 3 1) 2)
(check-expect (━ 30 7) 23)

(define addtup ; <-------------------------------- ADD_TUPPLE
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (✢ (car tup) (addtup (cdr tup)))))))

(check-expect (addtup '(1 2 3)) 6)

(define x ; <-------------------------------- MUTIPLY
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (✢ n (x n (sub1 m)))))))

(check-expect (x 2 5) 10)

(define tup+ ; <-------------------------------- ADD_ELEMENTS_IN_TUPPLE
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else
       (cons (✢ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(check-expect (tup+ '(1 2 3) '(3 2 1)) '(4 4 4))

(define tupbetter+ ; <-------------------------------- ADD_ELEMENTS_IN_TUPPLE_IMPROVED
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (✢ (car tup1) (car tup2)) (tupbetter+ (cdr tup1) (cdr tup2)))))))

(check-expect (tupbetter+ '(1 2 3) '(3 2)) '(4 4 3))

(define > ; <-------------------------------- IS_GREATER?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(check-expect (> 3 2) #t)
(check-expect (> 3 20) #f)
(check-expect (> 3 3) #f)

(define < ; <-------------------------------- IS_SMALLER?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(check-expect (< 3 2)  #f)
(check-expect (< 3 20) #t)
(check-expect (< 3 3)  #f)

(define o= ; <-------------------------------- IS_EQUAL? (MINE)
  (lambda (n m)
    (and (eq? (> n m) #f) (eq? (< n m) #f))))

(define = ; <--------------------------------- IS_EQUAL? (BOOK)
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))
  
(check-expect (o= 3 2)  #f)
(check-expect (o= 3 3)  #t)
(check-expect (=  3 2)  #f)
(check-expect (=  3 3)  #t)

(define ** ; <-------------------------------- EXPONENTIATION
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (** n (sub1 m)))))))

(check-expect (** 2 3) 8)
(check-expect (** 3 2) 9)



(test)