#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

;; 6
(define-syntax var
  (syntax-rules ()
    ((var w) (vector w))))

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(check-equal? x (vector 'x))
(check-equal? y (vector 'y))
(check-equal? z (vector 'z))

(define-syntax var?
  (syntax-rules ()
    ((var? w) (vector? w))))

(check-true (var? x))
(check-true (var? y))
(check-true (var? z))

;; 7
(define-syntax rhs
  (syntax-rules ()
    ((rhs a) (cdr a))))

(check-equal? (rhs `(,z . a)) 'a)

;; 8
(check-equal? (rhs `(,z . b)) 'b)

;; 9
(check-equal? (rhs `(,z . ,w)) w)

;; 10
(check-equal? (rhs `(,z . (,x e ,y))) `(,x e ,y))

;; 13
(define empty-s '())

;; 27
(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(assq v s) =>
           (lambda (a)
             (walk (rhs a) s))]
       [else v])]
    [else v]))

;; 14
(check-equal? (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a)

;; 15
(check-equal? (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a)

;; 16
(check-equal? (walk x `((,z . a) (,x . ,w) (,y . ,z))) w)

;; 17
(check-equal? (walk w `((,z . a) (,x . ,w) (,y . ,z))) w)

;; 18
; (walk x `((,x . ,y) (,z . ,x) (,y . ,z)))
; no answer...

;; 19
(check-equal? (walk w `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z))) 'b)

;; 23
(let ([e `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w))])
  (check-equal? (walk x e) 'b)
  (check-equal? (walk u e) 'b)
  (check-equal? (walk w e) 'b))

;; 24
(let ([e `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w))])
  (check-equal? (walk x e) z)
  (check-equal? (walk u e) z)
  (check-equal? (walk w e) z))

;; 25
(check-equal? (walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w))) `(,x e ,x))

;; 26
; because the list is not a fresh variable

;; 29
(define (ext-s x v s)
  (cons `(,x . ,v) s))

; (walk x (ext-s x y `((,z . ,x) (,y . ,z))))
; no answer...

;; 30
(check-equal? (walk y `((,x . e))) y)

;; 31
(check-equal? (walk y (ext-s y x `((,x . e)))) 'e)

;; 32
(check-equal? (walk x `((,y . ,z) (,x . ,y))) z)

;; 33
(check-equal? (walk x (ext-s z 'b `((,y . ,z) (,x . ,y)))) 'b)

;; 34
(check-equal? (walk x (ext-s z w `((,y . ,z) (,x . ,y)))) w)

;; 36
(define (unify v w s)
  (let ([v (walk v s)]
        [w (walk w s)])
    (cond
      [(eq? v w) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (cond
         [(unify (car v) (car w) s) =>
            (lambda (s)
              (unify (cdr v) (cdr w) s))]
         [else #f])]
      [(equal? v w) s]
      [else #f])))

;; 35
(check-equal? (unify v w empty-s) `((,v . ,w)))

;; 47
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s))]
      [else v])))

;; 44
(check-equal? (walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) '(a a c))

;; 52
(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(define-syntax size-s
  (syntax-rules ()
    ((size-s l) (length l))))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (ext-s v (reify-name (size-s s)) s)]
      [(pair? v) (reify-s (cdr v)
                          (reify-s (car v) s))]
      [else s])))

(check-equal? (reify-s v empty-s) `((,v . _.0)))

;; 53
(let ([r `(,w ,x ,y)])
  (check-equal? (walk* r (reify-s r empty-s)) '(_.0 _.1 _.2)))

;; 54
(let ([r (walk* `(,w ,x ,y) empty-s)])
  (check-equal? (walk* r (reify-s r empty-s)) '(_.0 _.1 _.2)))

;; 55
(let ([r `(,u (,v (,w ,x) ,y) ,x)])
  (check-equal? (walk* r (reify-s r empty-s)) '(_.0 (_.1 (_.2 _.3) _.4) _.3)))

;; 56
(let ([s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))])
  (let ([r (walk* x s)])
    (check-equal? (walk* r (reify-s r empty-s)) '(a _.0 c _.0))))

;; 58
(define (reify v)
  (walk* v (reify-s v empty-s)))

(let ([s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))])
  (check-equal? (reify (walk* x s)) '(a _.0 c _.0)))

;; 59
(define (occurs√ x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or
         (occurs√ x (car v) s)
         (occurs√ x (cdr v) s))]
      [else #f])))

;; 60
(define (unify√ v w s)
  (let ([v (walk v s)]
        [w (walk w s)])
    (cond
      [(eq? v w) s]
      [(var? v) (ext-s√ v w s)]
      [(var? w) (ext-s√ w v s)]
      [(and (pair? v) (pair? w))
       (cond
        [(unify√ (car v) (car w) s) =>
                                    (lambda (s)
                                      (unify√ (cdr v) (cdr w) s))]
        [else #f])]
      [(equal? v w) s]
      [else #f])))

(define (ext-s√ x v s)
  (cond
    [(occurs√ x v s) #f]
    [else (ext-s x v s)]))

;; 61
; (run 1 (x)
;   (== `(,x) x))
; no answer...

;; 62
(check-run 1 (q)
           (fresh (x)
             (== `(,x) x)
             (== #f q))
           => '(#f))

;; 63
(check-run 1 (q)
           (fresh (x y)
             (== `(,x) x)
             (== `(,y) x)
             (== #t q))
           => '(#t))

;; 64
(define (==√ v w)
  (lambda (s)
    (cond
      [(unify√ v w s) => s#]
      [else (u# s)])))

(check-run 1 (x)
           (==√ `(,x) x)
           => '())

;; 65
; (run 1 (x)
;   (fresh (y z)
;     (== x z)
;     (== `(a b ,z) y)
;     (== x y))
;   => '())
; no answer...

;; 66
(check-run 1 (x)
           (fresh (y z)
             (== x z)
             (== `(a b ,z) y)
             (==√ x y))
           => '())

;; 67
; (run 1 (x)
;   (== `(,x) x))
; no answer...
