#lang racket/base

(provide member?)

(require "lib/shared.rkt")

(module+ test
  (require rackunit))

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

(module+ test
  (check-true (mylat? '(a b c)))
  (check-false (mylat? '((a) b c d)))
  (check-false (mylat? '(a (b) c d)))
  (check-true (mylat? '()))
  (check-true (lat? '(a b c)))
  (check-false (lat? '((a) b c d)))
  (check-false (lat? '(a (b) c d)))
  (check-true (lat? '())))

;; pg 21 - 31

(module+ test
  (check-true (or (null? '()) (atom? '(a b c))))
  (check-true (or (null? '(a b c)) (null? '()))))

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

(module+ test
  (check-true (mymember? 'b '(a b c)))
  (check-false (mymember? 'd '(a b c)))
  (check-true (member? 'b '(a b c)))
  (check-false (member? 'd '(a b c))))
