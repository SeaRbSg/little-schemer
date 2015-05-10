#lang racket

(require minikanren)
(require rackunit)
(require "reasoned-prelude.rkt")

[check-equal?
  (let [(x (lambda (a) a)) ; identity fn
        (y 'c)]
    (x y))
  'c]

[check-equal?
  (run* (r)
    (fresh (y x)
      (== `(,x ,y) r)))
  '((_.0 _.1))]

;; I'm not typing the next one out...

[check-equal?
  (car '(grape rasin pear))
  'grape]

[check-equal?
  (car '(a c o r n))
  'a]

;; Skipping ahead a bit to grab the definition of caro
(define caro ; unify `head` to the head of `lst`
  (lambda (lst head)
    (fresh (tail)
      (== (cons head tail) lst))))

[check-equal?
  (run* (r)
    (caro '(a c o r n) r))
  '(a)]

[check-equal?
  (run* (q)
    (caro '(a c o r n) 'a)
    (== #t q))
  '(#t)]

;; Skipping a bunch of these, because cdro is so similar to caro
(define cdro
  (lambda (lst tail)
    (fresh (head)
      (== (cons head tail) lst))))

[check-equal?
  (run* (l)
    (fresh (x)
      (cdro l '(c o r n)) ; l is a list with a fresh variable head
      (caro l x)
      (== 'a x)))
  '((a c o r n))]

(define conso
  (lambda (head tail lst)
    (== (cons head tail) lst)))

[check-equal?
  (run* (l)
    (conso '(a b c) '(d e) l))
  '(((a b c) d e))]


(define nullo
  (lambda (lst)
    (== '() lst)))

;; Man, "what is the value of"  vs. "what is the value associated with"
;; has gotten me a few times!

[check-equal?
  (run* (q)
    (nullo '(grape raisin pear))
    (== #t q))
  '()]

[check-equal?
  (run* (x)
    (nullo x))
  '(())]

(define eqo ;; this is just an alias for `unify`...
  (lambda (a b)
    (== a b)))

(define pairo
  (lambda (pair)
    (fresh (head tail)
      (conso head tail pair))))

[check-equal?
  (run* (q)
    (pairo '())
    (== q #t))
  '()]

[check-equal?
  (run* (x)
    (pairo x))
  '((_.0 . _.1))]

(define cdro-by-cons
  (lambda (lst tail)
    (fresh (head)
      (conso head tail lst))))

(define caro-by-cons
  (lambda (lst head)
    (fresh (tail)
      (conso head tail lst))))

(define pairo-by-cons
  (lambda (pair)
    (fresh (tail)
      (conso head tail lst))))

