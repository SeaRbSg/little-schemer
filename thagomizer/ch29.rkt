#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

;; 6
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

;; 8
[check-equal? (rhs `(,z . b)) 'b]

;; 9
(define w (var 'w))
[check-equal? (rhs `(,z . ,w)) w]

;; 10
[check-equal? (rhs `(,z . (,x e ,y))) `(,x e ,y)]

(define empty-s '())

;; 14
[check-equal? (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a]

;; 15
[check-equal? (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a]

;; 16
[check-equal? (walk x `((,z . a) (,x . ,w) (,y . ,z))) w]

;; 17
[check-equal? (walk w `((,x . ,y) (,z . ,x) (,y . ,z))) w]

;; 18
;; [check-equal? (walk x `((,x . ,y) (,z . ,x) (,y . ,z))) '()]

;; 19
[check-equal? (walk w `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z))) 'b]

;; 23
(define u (var 'u))
(define v (var 'v))
[check-equal? (walk u `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk v `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk w `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk x `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]

;; 24
[check-equal? (walk u `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk v `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk w `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk x `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]

;; 25
[check-equal? (walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w))) `(,x e ,x)]

;; 27
(define my-walk
  (lambda (v s)
    (cond
     [(var? v)
      (cond
       [(assq v s) =>
        (lambda (a)
          (my-walk (rhs a) s))]
       [else v])]
     [else v])))

;; 29
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

;; 30
[check-equal? (my-walk y `((,x . e))) y]

;; 31
[check-equal? (my-walk y (ext-s y x `((,x . e)))) 'e]

;; 32
[check-equal? (my-walk x `((,y . ,z) (,x . ,y))) z]

;; 33
[check-equal? (my-walk x (ext-s z 'b `((,y . ,z) (,x . ,y)))) 'b]

;; 34
[check-equal? (my-walk x (ext-s z w `((,y . ,z) (,x . ,y)))) w]

;; 35
[check-equal? (unify v w s) '()]
      
       
