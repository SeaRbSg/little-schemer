#lang racket
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

(define s# (== #f #f))
(define u# (== #f #t))

[check-equal?
 (let ((x (lambda (a) a)) ; x is a function that returns the argument
       (y 'c))            ; y is c
   (x y))                 ; call x with y ==> return y ==> c
 'c]

[check-equal?
  (run* (r)
        (fresh (y x)
               (== (cons x (cons y '())) r)))
  '((_.0 _.1))]

[check-equal?
 (run* (r)
       (fresh (v w)
              (== (let ((x v) (y w)) (cons x (cons y '()))) r)))
 '((_.0 _.1))]

;-------------------------------------------------------------------------------

(define caro
  (lambda (list r)
    (fresh (x)
           (== (cons r x) list))))

; x is fresh (empty, irrelevant, just for building the helping construct)
; (cons r x) ==> make a list where the first element is r ==> (r _.0)
;                it doesnt matter if it is (r . _.0) or (r whatever)
; (== (cons r x) l) => (== (r _.0) l) => (== (r ...) (car_of_l cdr_of_l)) => r is bound to car of l!!!

; QUESTION: therefore can we assume that the freshy x is bound to the cdr of l (inside that fresh box)?

[check-equal?
 (run* (r)
       (caro '(a c o r n) r))
 '(a)]

[check-equal?
 (run* (r)
       (fresh (x y)
              (caro '(grape raisin pear) x)
              (caro '((a) (b) (c)) y)
              (== (cons x y) r)))
 '((grape a))]

; the key is that x and y, by being fresh variables, can be atoms, lists or functions, and can always be consed
; if instead of ((a) (b) (c)) we had (a b c), the r would be ((grape . a)), so even unnaturally they can be consed

(define cdro
  (lambda (list r)
    (fresh (x)
           (== (cons x r) list))))

; obvious!

[check-equal?
 (run* (r)
       (fresh (v)
              (cdro '(a c o r n) v)
              (caro v r)))
 '(c)]

[check-equal?
 (run* (r)
       (fresh (x y)
              (cdro '(grape raisin pear) x)
              (caro '((a) (b) (c)) y)
              (== (cons x y) r)))
 '(((raisin pear) a))]

; so far so good

;-------------------------------------------------------------------------------

[check-equal?
 (run* (q)
       (cdro '(a c o r n) '(c o r n)) ; goal succeeds because it is true
       (== #t q))                     ; goal succeeds because q is fresh
 '(#t)]

[check-equal?
 (run* (x) (cdro '(c o r n) (cons x '(r n)))) ; (o r n) <-> (x r n) => o <-> x
 '(o)]

[check-equal?
 (run* (l)
       (fresh (x)
              (cdro l '(c o r n))  ; l is (? c o r n)
              (caro l x)           ; l is (x ...) + previous => l is (x c o r n)
              (== 'a x)))          ; since x is a => l is (a c o r n)
 '((a c o r n))]

;-------------------------------------------------------------------------------

(define conso
  (lambda (l1 l2 r)
    (== (cons l1 l2) r)))

[check-equal?
 (run* (l) (conso '(a b c) '(d e) l))
 '(((a b c) d e))]

[check-equal?
 (run* (x)
       (conso x '(a b c) '(d a b c)))
 '(d)]

[check-equal?
 (run* (r)
       (fresh (x y z)
              (== (cons 'e (cons 'a (cons 'd x))) r)
              (conso y (cons 'a (cons z '(c))) r)))
 '((e a d c))]

[check-equal?
 (run* (x)
       (conso x (cons 'a (cons x '(c))) (cons 'd (cons 'a (cons x '(c))))))
 '(d)]

; enough bs! the beans example [29] I am not even going to do <%= tantrum!! %>

;-------------------------------------------------------------------------------

(define nullo
  (lambda (l)
    (== l '())))

[check-equal?
 (run* (q)
       (nullo '(pepe juan))
       s#)
 '()]

[check-equal?
 (run* (q)
       (nullo q)) ; Ha! because this goal succeeds => q is bound to '() !!!
 '(())]

;-------------------------------------------------------------------------------

; IMPORTANT: caro cdro nullo are all goals like ==, s# and u#

(define eqo
  (lambda (x y)
    (== x y)))  ; in this manner eqo sees if two things can be bound, or eqo == ==

[check-equal?
 (run* (r)
       (fresh (x y)
              (== (cons x (cons y 'salad)) r)))
 '((_.0 _.1 . salad))]

(define pairo
  (lambda (list)
    (fresh (x y)
           (conso x y list)))) ; pairo of l succeeds if l can be built with conso

[check-equal?
 (run* (q)
       (pairo (cons q q)) ; succeeds because (cons q q) => (q . q)
       (== #t q))
 '(#t)]

[check-equal?
 (run* (q)
       (pairo '()) ; goal fails because () cannot be made with conso, also because there is no car and cdr
       (== #t q))
 '()]

[check-equal?
 (run* (q)
       (pairo 'pair) ; also fails for the same reason as above
       (== #t q))
 '()]

[check-equal?
 (run* (z) (pairo z)) ; OMG! succeeds because of how pairo executes!! by introducing 2 fresh vars
 '((_.0 . _.1))]

; (pairo z)
; does (fresh (x y) (conso x y z)) => conso of 3 fresh vars. And conso x y z does
; (conso x y z) => (== (cons x y) z) => (== (x . y) z)
; rearviewing we have (pairo z) ==> (== (x . y) z) ==> z IS bound to (x . y) where all are fresh variables
; so (pairo z) not only succeeds but z is bound to two fresh vars!!

[check-equal?
 (run* (r) (pairo (cons r 'pear)))
 '(_.0)]

; (cons r 'pear) => (r . 'pear)
; (pairo (cons r 'pear)) => (pairo (r . 'pear))
; if z == (r . pear), we know that z is re-bound to 2 fresh vars (x y) (_.0 _.1)
; so (r . pear) == (_.0 _.1) => r == _.0

; And to finish it off here are the defs of caro and cdro using consos

(define caro_mio
  (lambda (list r)
    (fresh (x)
           (conso r x list))))

(define CDRO
  (lambda (list r)
    (fresh (x)
           (conso x r list))))
