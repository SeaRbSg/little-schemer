#lang racket/base

(provide member?)

(require rackunit)
(require "lib/shared.rkt")

;;; Chapter 2
;; pg 15 - 20

(define mylat?
  (lambda (x)
    (or (null? x)
        (and (list? x)
             (atom? (car x))
             (mylat? (cdr x))))))

(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

(check-equal? (mylat? '(a b c))
              #t)
(check-equal? (mylat? '((a) b c d))
              #f)
(check-equal? (mylat? '(a (b) c d))
              #f)
(check-equal? (mylat? '())
              #t)
(check-equal? (lat? '(a b c))
              #t)
(check-equal? (lat? '((a) b c d))
              #f)
(check-equal? (lat? '(a (b) c d))
              #f)
(check-equal? (lat? '())
              #t)

;; pg 21 - 31

(check-equal? (or (null? '()) (atom? '(a b c)))
              #t)
(check-equal? (or (null? '(a b c)) (null? '()))
              #t)

(define mymember?
  (lambda (a lat)
    (and (not (null? lat))
         (or (and (atom? (car lat)) (eq? a (car lat)))
             (mymember? a (cdr lat))))))

(define member?
  (lambda (a lat)
    (cond [(null? lat) #f]
          [else (or (eq? (car lat) a)
                    (member? a (cdr lat)))])))

(check-equal? (mymember? 'b '(a b c))
              #t)
(check-equal? (mymember? 'd '(a b c))
              #f)
(check-equal? (member? 'b '(a b c))
              #t)
(check-equal? (member? 'd '(a b c))
              #f)
