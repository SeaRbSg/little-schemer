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

(define // ; <-------------------------------- DIVISION
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (// (━ n m) m))))))
      ;(else (sub1 (━ n (sub1 m))))

(check-expect (// 15 3) 5)
(check-expect (// 15 4) 3)

(define length ; <-------------------------------- LENGTH
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(check-expect (length '(hotdogs with mustard sauerkraut and pickles)) 6)

(define pick ; <-------------------------------- []
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(check-expect (pick 1 '(10 20 30)) 10)
(check-expect (pick 3 '(10 20 30)) 30)

(define rempick ; <-------------------------------- DELETE_AT
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(check-expect (rempick 1 '(10 20 30)) '(20 30))
(check-expect (rempick 3 '(10 20 30)) '(10 20))

(check-expect (number? 76) #t)
(check-expect (number? 'pepe) #f)

(define no-nums ; <-------------------------------- REMOVE_NUMBERS
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else 
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))
  
(check-expect (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

(define all-nums ; <-------------------------------- EXTRACT_NUMBERS
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(check-expect (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

(define eqan? ; <-------------------------------- SAME_ATOM?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(check-expect (eqan? 2 2) #t)
(check-expect (eqan? 2 3) #f)
(check-expect (eqan? 'pepe 'pepe) #t)
(check-expect (eqan? 'pepe 'juan) #f)
(check-expect (eqan? 2 'pepe) #f)
;(check-expect (eqan? 2 '2) #f) ; <=============== FAILS :: INTERESTING
; (atom? 2) == (atom? '2) == (num? '2) == (num? 2) == true

(define occur ; <-------------------------------- OCCURRENCES
  (lambda (a lat)
    (cond 
      ((null? lat) 0)
      (else
       (cond
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(check-expect (occur 1 '(2 3 1 1 4 1)) 3)
(check-expect (occur 7 '(2 3 1 1 4 1)) 0)

(define one? ; <-------------------------------- IS_ONE?
  (lambda (n)
    (eqan? n 1))) ; <============================= WHY NOT THIS WAY?

(check-expect (one? 1) #t)
(check-expect (one? 2) #f)
(check-expect (one? 'pepe) #f)

(define rempick2 ; <-------------------------------- DELETE_AT (revised)
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

(check-expect (rempick2 1 '(10 20 30)) '(20 30))
(check-expect (rempick2 3 '(10 20 30)) '(10 20))

(test)