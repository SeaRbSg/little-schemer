#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


(test-case "pages 59 & 60"
           [check-true (atom? 14)]
           [check-true (number? -3)]
           [check-true (number? 3.14159)]
           [check-eq? (add1 67) 68]
           [check-eq? (sub1 5) 4]
           [check-true (zero? 0)]
           [check-false (zero? 1492)])


;; This was my first attempt
;; (define o+ 
;;   (lambda (x y)
;;     (cond
;;      ((zero? x) y)
;;      (else (o+ (sub1 x) (add1 y))))))

;; This is the book's definition. Both work.
(define o+
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (add1 (o+ x (sub1 y)))])))

(test-case "o+"
           [check-eq? (o+ 46 12) 58])


(define o-
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (sub1 (o- x (sub1 y)))])))

(test-case "o-"
           [check-eq? (o- 14 3) 11]
           [check-eq? (o- 17 9) 8]
           [check-eq? (o- 18 25) -7])


(define tup?
  (lambda (tup)
    (cond
     [(null? tup) #t]
     [(number? (car tup)) (tup? (cdr tup))]
     [else #f])))

(test-case "tup?"
           [check-true (tup? '())] 
           [check-true (tup? '(2 11 3 79 47 6))]
           [check-true (tup? '(8 55 5 555))]
           [check-false (tup? '(1 2 8 apple 4 3))]
           [check-false (tup? '(3 (7 4) 13 9))])

(define addtup
  (lambda (tup)
    (cond
     [(null? tup) 0]
     [else (o+ (car tup) (addtup (cdr tup)))])))

(test-case "addtup"
           [check-eq? (addtup '(3 5 2 8)) 18]
           [check-eq? (addtup '(15 6 7 12 3)) 43])


(define ×
  (lambda (m n)
    (cond
     [(zero? n) 0]
     [else (o+ m (× m (sub1 n)))])))

(test-case "×"
           [check-eq? (× 5 3) 15]
           [check-eq? (× 13 4) 52])

(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))

(test-case "tup+"
           [check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11)]
           [check-equal? (tup+ '(2 3) '(4 6)) '(6 9)]
           [check-equal? (tup+ '(3 7) '(4 6)) '(7 13)]
           [check-equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1)])


(define o>
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (o> (sub1 n) (sub1 m))])))

(test-case "o>"
           [check-false (o> 12 133)]
           [check-true  (o> 120 11)]
           [check-false (o> 3 3)])


(define o<
  (lambda (n m)
    (cond
     [(zero? m) #f]
     [(zero? n) #t]
     [else (o< (sub1 n) (sub1 m))])))

(test-case "o<"
           [check-true  (o< 4 6)]
           [check-false (o< 8 3)]
           [check-false (o< 6 6)])

(define o=
  (lambda (n m)
    (cond
     [(< m n) #f]
     [(> m n) #f]
     [else #t])))

(test-case "o="
           [check-true (o= 6 6)]
           [check-false (o= 2 3)]
           [check-false (o= 4 1)])


(define ↑
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (× n (↑ n (sub1 m)))])))

(test-case "↑"
           [check-eq? (↑ 1 1) 1]
           [check-eq? (↑ 2 3) 8]
           [check-eq? (↑ 5 3) 125])


(define ÷
  (lambda (n m)
    (cond
     [(< n m) 0]
     [else (add1 (÷ (o- n m) m))])))

(test-case "÷"
           [check-eq? (÷ 15 3) 5]
           [check-eq? (÷ 15 4) 3])


(define my_length
  (lambda (l)
    (cond
     [(null? l) 0]
     [else (add1 (my_length (cdr l)))])))

(test-case "my_length"
           [check-eq? (my_length '(hotdogs with mustard sauerkraut and pickles)) 6]
           [check-eq? (my_length '(ham and cheese on rye)) 5])


(define my_pick
  (lambda (n lat)
    (cond
     [(null? lat) '()]
     [(zero? (sub1 n)) (car lat)]
     [else (my_pick (sub1 n) (cdr lat))])))

(test-case "my_pick"
           [check-eq? (my_pick 4 '(lasagna spaghetti ravioli macaroni meatball))
                      'macaroni]
           [check-eq? (my_pick 0 '(a)) '()])


(define rempick
  (lambda (n lat)
    (cond
     [(null? lat) '()]
     [(zero? (sub1 n)) (cdr lat)]
     [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))


(test-case "rempick"
           [check-equal? (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard)])


(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(number? (car lat)) (no-nums (cdr lat))]
     [else (cons (car lat) (no-nums (cdr lat)))])))

(test-case "no-nums"
           [check-equal? (no-nums '(5 pears 6 prunes 9 dates)) 
                         '(pears prunes dates)]
           [check-equal? (no-nums '(1 2 3)) '()])


(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(not (number? (car lat))) (all-nums (cdr lat))]
     [else (cons (car lat) (all-nums (cdr lat)))])))

(test-case "all-nums"
           [check-equal? (all-nums '(5 pears 6 prunes 9 dates)) 
                         '(5 6 9)]
           [check-equal? (all-nums '(pears berries dates)) '()])


(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))

(test-case "eqan?"
           [check-true (eqan? 3 3)]
           [check-true (eqan? 'a 'a)]
           [check-false (eqan? 1 2)]
           [check-false (eqan? 'a 'b)])


(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eq? a (car lat)) (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

(test-case "occur"
           [check-equal? (occur 'a '(a b a c a)) 3]
           [check-equal? (occur 'a '(a b)) 1]
           [check-equal? (occur 'a '()) 0]
           [check-equal? (occur 'a '(b c d)) 0])


;; My initial definition of one?
;; (define one?
;;   (lambda (n)
;;     (cond
;;      [(zero? (sub1 n)) #t]
;;      [else #f])))


(define one?
  (lambda (n)
    (= n 1)))

(test-case "one?"
           [check-true (one? 1)]
           [check-false (one? 3)]
           [check-false (one? 0)])


(define rempick2
  (lambda (n lat)
    (cond
     [(null? lat) '()]
     [(one? n) (cdr lat)]
     [else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))])))


(test-case "rempick2"
           [check-equal? (rempick2 3 '(hotdogs with hot mustard)) '(hotdogs with mustard)])
