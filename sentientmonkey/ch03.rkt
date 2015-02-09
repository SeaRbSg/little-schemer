#lang racket/base

(require rackunit)
(require "prelude.rkt")
(provide multirember)

; Chapter 3

(provide firsts)

; rember final version
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) (quote ())]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat)
                  (rember a
                          (cdr lat)))])))

(check-equal? (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
(check-equal? (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))

; firsts
(define firsts
  (lambda (l)
    (cond
      ((null? l ) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(check-equal? (firsts '((a b) (c d) (e f))) '(a c e))

(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) (quote ())]
      [(eq? (car lat) old)
       (cons old
             (cons new (cdr lat)))]
      [else (cons (car lat)
                  (insertR new old
                           (cdr lat)))])))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with fudge topping for desert))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) (quote ())]
      [(eq? (car lat) old)
       (cons new lat)]
      [else (cons (car lat)
                  (insertL new old
                           (cdr lat)))])))

(check-equal? (insertL 'fudge 'topping '(ice cream with topping for desert))
              '(ice cream with fudge topping for desert))

; subst
(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) (quote ())]
      [(eq? (car lat) old)
       (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst new old
                         (cdr lat)))])))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with topping for desert))

; subst2 refactored
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) (quote ())]
      [(or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst2 new o1 o2
                          (cdr lat)))])))

(check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))

; multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

; multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old
                                (cdr lat)))))))

(check-equal? (multiinsertR 'vanilla 'chocolate '(chocolate ice cream sundae with chocolate sauce))
              '(chocolate vanilla ice cream sundae with chocolate vanilla sauce))

; multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old
                                (cdr lat)))))))

(check-equal? (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
              '(chips and fried fish or fried fish and fried))

; multisubst
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (multisubst new old
                         (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old
                              (cdr lat)))))))

(check-equal? (multisubst 'fried 'fish '(chips and fish or fish and fried))
              '(chips and fried or fried and fried))
