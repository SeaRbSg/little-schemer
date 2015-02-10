#lang racket
(require "lib/shared.rkt")
;(require "ch6.rkt")
;(require "ch7.rkt")
(require rackunit)
(require racket/trace)

; an entry => 2 lists, the first one is a set of keys, the second one of the same legth as the first => the values
'((lang color) '((ruby color) (lapislazuli blue)))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (cdr l)))
    

(define lookup-in-entry
  (lambda (name keys values not-found)
    (cond
      [(null? keys) (not-found name)]
      [(eq? name (car keys)) (car values)]
      [else (lookup-in-entry name (cdr keys) (cdr values) not-found)])))
    

(module+ test
  (check-equal? (lookup-in-entry 'entree '(appetizer entree beverage) '(food tastes good) #f) 'tastes))
  
; a table => a list of entries
'(((key1 key2) '((a b) (lapislazuli blue)))
  ((key6 key9) '((a c) (pepelui amaretto))))

(define lookup-in-table
  (lambda (name table not-found-t)
    (cond
      [(null? table) (not-found-t name)]
      [else (lookup-in-entry name (car table) 
                             (lambda (name)
                               (lookup-in-table name (cdr table) not-found-t)))])))

;(define expression-to-action
;  (lambda (e)
;    (cond
;      [(atom? e) (atom-to-action e)]
;      [else (list-to-action e)])))

(define atom-to-action
  (lambda (e)
    (cond
      [(number? e) *const]
      [else #t])))

(atom-to-action 5)

(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build (quote primitive) e)])))