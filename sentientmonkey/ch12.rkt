#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch03.rkt")
(require "ch04.rkt")
(require "ch08.rkt")
(require "ch09.rkt")

(check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))

(define (multirember-Y a lat)
  ((Y (lambda (mr)
        (lambda (lat)
          (cond
            [(null? lat) '()]
            [(eq? a (car lat)) (mr (cdr lat))]
            [else (cons (car lat) (mr (cdr lat)))]))))
   lat))

(check-equal? (multirember-Y 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))

(define (multirember-let a lat)
  (letrec
     ([mr (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? a (car lat)) (mr (cdr lat))]
              [else (cons (car lat) (mr (cdr lat)))]))])
     (mr lat)))

(check-equal? (multirember-let 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))

(check-equal? (multirember-let 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte))

(define rember-eq? (rember-f eq?))

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [(eq? (car lat) a) #t]
    [else (member? a (cdr lat))]))

(check-false (member? 'ice '(salad greens with pears brie cheese frozen yogurt)))

(define (member-let? a lat)
  (letrec
    ([yes? (lambda (l)
             (cond
               [(null? l) #f]
               [(eq? (car l) a) #t]
               [else (yes? (cdr l))]))])
     (yes? lat)))

(check-false (member-let? 'ice '(salad greens with pears brie cheese frozen yogurt)))

(define (union set1 set2)
  (letrec
    ([U (lambda (s)
          (cond
            [(null? s) set2]
            [(M? (car s) set2) set2]
            [else (cons (car s) (U (cdr s)))]))]
     [M? (lambda (a lat)
           (letrec
             ([N? (lambda (lat)
                    (cond
                      [(null? lat) #f]
                      [(eq? (car lat) a) #t]
                      [else (N? (cdr lat))]))])
             (N? lat)))])
    (U set1)))

(check-equal? (union '(tomatoes and macaroni cheese) '(macaroni and cheese)) '(tomatoes macaroni and cheese))

(define two-in-a-row?
  (letrec
    ([T? (lambda (a lat)
           (cond [(null? lat) #f]
                 [else (or (eq? (car lat) a)
                           (T? (car lat) (cdr lat)))]))])
    (lambda (lat)
      (cond
        [(null? lat) #f]
        [else (T? (car lat) (cdr lat))]))))

(check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
(check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)

(define (sum-of-prefixes tup)
  (letrec
    ([S (lambda (ssof tup)
          (cond [(null? tup) '()]
                [else (cons (+ ssof (car tup))
                            (S (+ ssof (car tup)) (cdr tup)))]))])
    (S 0 tup)))

; I think I prefer the sum-of-prefixes style over the lambda defined inside the
; letrec. Makes it much easier to see arguments for the define using the condensed
; racket style.

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
(check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

(define (scramble tup)
  (letrec
    ([P (lambda (tup rev-pre)
          (cond
            [(null? tup) '()]
            [else (cons (pick (car tup)
                              (cons (car tup) rev-pre))
                        (P (cdr tup) (cons (car tup) rev-pre)))]))])
    (P tup '())))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
(check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1))
(check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2))
