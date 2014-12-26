#lang racket

(require "lib/shared.rkt")
(require rackunit)

; our friend rember from ch 5
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) lat]
      [(eq? a (car lat)) (rember a (cdr lat))]
      [else (cons (car lat) (rember a (cdr lat)))])))

(module+ test (check-equal? (rember 'a '(b c a)) '(b c)))

(define rember-f ; intuitive enough
  (lambda (test? a lis)
    (cond
      [(null? lis) lis]
      [(test? a (car lis)) (rember-f test? a (cdr lis))]
      [else (cons (car lis) (rember-f test? a (cdr lis)))])))

(module+ test 
  (check-equal? (rember-f = '5 '(6 2 5 3)) '(6 2 3))
  (check-equal? (rember-f eq? 'a '(b c a)) '(b c))
  (check-equal? (rember-f eq? 'jelly '(jelly beans are good)) '(beans are good))
  (check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake))))

; equal? works with lists // eq? with atoms // = with numbers

(define eq?-c ; just another function where argument is a, but now it returns another function (with a set)
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(module+ test
  [check-true ((eq?-c 'salad) 'salad)]
  [check-false ((eq?-c 'salad) 'pepe)])

(define rember_f
  (lambda (test?)
    (lambda (a lis)
      (cond
        [(null? lis) lis]
        [(test? a (car lis))  ((rember_f test?) a (cdr lis))]
        [else (cons (car lis) ((rember_f test?) a (cdr lis)))]))))

(module+ test 
  (check-equal? ((rember_f =) '5 '(6 2 5 3)) '(6 2 3))
  (check-equal? ((rember_f eq?) 'a '(b c a)) '(b c))
  (check-equal? ((rember_f eq?) 'jelly '(jelly beans are good)) '(beans are good))
  (check-equal? ((rember_f equal?) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))
  (check-equal? ((rember_f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertL ; <--------------------- INSERT_L from ch 3
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons new (cons old (cdr lat)))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define insertR ; <--------------------- INSERT_R from ch 3
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(module+ test
  (check-equal? (insertL 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? (insertL 9 1 '(1)) '(9 1))
  (check-equal? (insertL 9 1 '(2 3 4 5 6)) '(2 3 4 5 6))
  (check-equal? (insertR 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? (insertR 9 1 '(1)) '(1 9))
  (check-equal? (insertR 9 1 '(2 3 4 5 6)) '(2 3 4 5 6)))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(test? old (car l)) (cons new (cons old (cdr l)))]
        [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(test? old (car l)) (cons old (cons new (cdr l)))]
        [else (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

(module+ test
  (check-equal? ((insertL-f eq?) 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? ((insertL-f eq?) 9 1 '(1)) '(9 1))
  (check-equal? ((insertL-f eq?) 9 1 '(2 3 4 5 6)) '(2 3 4 5 6))
  (check-equal? ((insertR-f eq?) 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? ((insertR-f eq?) 9 1 '(1)) '(1 9))
  (check-equal? ((insertR-f eq?) 9 1 '(2 3 4 5 6)) '(2 3 4 5 6)))

(define seqL
  (lambda (new old l)
    (cons new (cons old (cdr l)))))

(define seqR
  (lambda (new old l)
    (cons old (cons new (cdr l)))))

(define insert-g  ; <----------------- head exploding, come back and review again
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(eq? old (car l)) (seq new old (cdr l))]
        [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(define insertL#
  (insert-g seqL))

(define insertR#
  (insert-g seqR))

(define insertL_
  (insert-g
    (lambda (new old l)
      (cons new (cons old (cdr l))))))

(define insertR_
  (insert-g
    (lambda (new old l)
      (cons old (cons new (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst ; <--------------------- SUBST
  (lambda (new old l)
    (cond
      [(null? l) l]
      [(eq? old (car l)) (cons new (cdr l))]
      [else (cons (car l) (subst new old (cdr l)))])))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define substS
  (insert-g seqS))

(module+ test
  (check-equal? (substS 'x 2 '(1 2 3 4)) '(1 x 3 4))
  (check-equal? (substS 9 1 '(1)) '(9))
  (check-equal? (substS 9 1 '(2 3 4 5 6)) '(2 3 4 5 6)))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define seqrem
  (lambda (new old l)
    l))

;; overload !!!
