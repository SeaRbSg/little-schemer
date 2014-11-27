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
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

(test (mylat? '(a b c))
      #t)
(test (mylat? '((a) b c d))
      #f)
(test (mylat? '(a (b) c d))
      #f)
(test (mylat? '())
      #t)
(test (lat? '(a b c))
      #t)
(test (lat? '((a) b c d))
      #f)
(test (lat? '(a (b) c d))
      #f)
(test (lat? '())
      #t)

;; pg 21 - 31

(test (or (null? '()) (atom? '(a b c)))
      #t)
(test (or (null? '(a b c)) (null? '()))
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

(test (mymember? 'b '(a b c))
      #t)
(test (mymember? 'd '(a b c))
      #f)
(test (member? 'b '(a b c))
      #t)
(test (member? 'd '(a b c))
      #f)
