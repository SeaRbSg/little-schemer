#lang racket/base

(provide member?)

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
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(test #t (mylat? '(a b c)))
(test #f (mylat? '((a) b c d)))
(test #f (mylat? '(a (b) c d)))
(test #t (mylat? '()))
(test #t (lat? '(a b c)))
(test #f (lat? '((a) b c d)))
(test #f (lat? '(a (b) c d)))
(test #t (lat? '()))

;; pg 21 - 31

(test #t (or (null? '()) (atom? '(a b c))))
(test #t (or (null? '(a b c)) (null? '())))

(define mymember?
  (lambda (a lat)
    (and (not (null? lat))
         (or (and (atom? (car lat)) (eq? a (car lat)))
             (mymember? a (cdr lat))))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

(test #t (mymember? 'b '(a b c)))
(test #f (mymember? 'd '(a b c)))
(test #t (member? 'b '(a b c)))
(test #f (member? 'd '(a b c)))
