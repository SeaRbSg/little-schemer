#lang racket/base

(provide multirember firsts)

(require rackunit)
(require "lib/shared.rkt")

;;; Chapter 3
;; pg 33 - 42

(define myrember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat)
                 (myrember a (cdr lat)))])))

(define rember1
  (lambda (a lat)
    [cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) a) (cdr lat)]
            [else (cons (car lat)
                        (rember1 a (cdr lat)))])]]))

(check-equal? (myrember 'b '(a b c))
              '(a c))
(check-equal? (myrember 'd '(a b c))
              '(a b c))
(check-equal? (rember1  'b '(a b c))
              '(a c))
(check-equal? (rember1  'd '(a b c))
              '(a b c))

;; pg 43 - 46

(define firsts
  (lambda (l)
    (cond [(null? l) '()]
          [else (cons (caar l)
                      (firsts (cdr l)))])))

(check-equal? (firsts '((a b) (c d) (e f)))
              '(a c e))
(check-equal? (null? (firsts '()))
              #t)
(check-equal? (firsts '((a b) (c) (d e f)))
              '(a c d))

;; pg 47

(define insertR
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons old
                                     (cons new
                                           (cdr lat)))]
          [else (cons (car lat)
                      (insertR new old (cdr lat)))])))

(check-equal? (insertR 'z 'c '(a b c d e))
              '(a b c z d e))
(check-equal? (insertR 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))

;; pg 51

(define insertL
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new
                                     (cons old
                                           (cdr lat)))]
          [else (cons (car lat)
                      (insertL new old (cdr lat)))])))

(check-equal? (insertL 'z 'c '(a b c d e))
              '(a b z c d e))
(check-equal? (insertL 'e 'd '(a b c d f g d h))
              '(a b c e d f g d h))

(define subst
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new
                                     (cdr lat))]
          [else (cons (car lat)
                      (subst new old (cdr lat)))])))

(check-equal? (subst 'z 'b '(a b c))
              '(a z c))

(define multirember
  (lambda (a lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) a) (multirember a (cdr lat))]
          [else (cons (car lat)
                      (multirember a (cdr lat)))])))

(check-equal? (multirember 'b '(b a b c b d b e b))
              '(a c d e))

(define multiinsertR
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old)
           (cons old
                 (cons new
                       (multiinsertR new old (cdr lat))))]
          [else (cons (car lat)
                      (multiinsertR new old (cdr lat)))])))

(check-equal? (multiinsertR 'z 'b '(a b c b d b))
              '(a b z c b z d b z))
