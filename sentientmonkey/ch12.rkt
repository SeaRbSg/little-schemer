#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch03.rkt")
(require "ch07.rkt")
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


(check-equal? (union '(tomatoes and macaroni cheese) '(macaroni and cheese)) '(tomatoes macaroni and cheese))
