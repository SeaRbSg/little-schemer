;; I GET IT! 'd' is the 'decrement' part, like in 'cdr',
;; and 'a' is the 'address' part, like in 'car'.
#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define five-floorso
  (lambda (lst)
    (fresh (a b c d e)
      (== lst `(,a ,b ,c ,d ,e)))))

(define not-topo
  (lambda (person lst)
    (conde
      [(== person (car lst)) %u]
      [else %s])))

(define not-bottomo
  (lambda (person lst)
    (conde
      [(== person (car (cdr (cdr (cdr (cdr lst)))))) %u]
      [else %s])))

(define highero
  (lambda (person1 person2 lst)
    (conde
      [(nullo lst) %u]
      [(highero person1 person2 (cdr lst)) %s]

(define not-adjacento
  (lambda (person1 person2 lst)
    (conde
      [(== person (car lst)) %u]
      [else %s])))


(run 1 (x)
  (five-floorso x)
  (not-topo 'adam x)
  (not-bottomo 'bill x)
  (not-topo 'cora x)
  (not-bottomo 'cora x)
  (highero 'dale 'bill x)
  (not-adjacento 'erin 'cora x)
  (not-adjacento 'cora 'bill x))

