#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define var vector)
(define var? vector?)

(define x (var 'x))

(define lhs car)
(define rhs cdr)
;; (rhs (z . 'b)) ; unbound identifier

(define empty-s '())

(define walk
  (lambda (v subs)
    (cond
      [(var? v)
       (cond
         [(assq v subs) => ;; Need more info about the arrow operator!
            (lambda (a)
              (walk (rhs a) subs))]
         [else v])]
      [else v])))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define unify
  (lambda (v w s)
    (let [(v (walk v s))  ; Find final substitutions for
          (w (walk w s))] ; v and w
      (cond
        [(eq? v w) s]     ; v and w are already unified
        [(var? v) (ext-s v w s)] ; fresh v
        [(var? w) (ext-s w v s)] ; fresh w
        [(and (pair? v) (pair? w)) ; unify cars
         (cond
           [(unify (car v) (car w) s) =>
            (lambda (s)
              (unify (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s] ; Don't understand this one, shouldn't it be '(pair) ?
        [else #f]))))

(define walk* ;; the difference-- ??
  (lambda (v s)
    (let [(v (walk v s))]
      (cond
        [(var? v) v]
        [(pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s))]
        [else v]))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" " " (number->string n)))))

(define reify-s
  (lambda (v s)
    (let [(v (walk v s))]
      (cond
        [(var? v)
         (ext-s v (reify-name (size-s)) s)]
        [(pair? v) (reify-s (cdr v)
                            (reify-s (car v) s))]
        [else s]))))

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(define ext-s√
  (lambda (x v s)
    (cond
      [(occurs√ x v s) #f]
      [else (ext-s x v s)])))

(define occurs√
  (lambda (x v s)
    (let [(v (walk v s))]
      (cond
        [(var? v) (eq? v x)]
        [(pair? v)
         (or
           (occurs√ x (car v) s)
           (occurs√ x (cdr v) s))]
        [else #f]))))

(define unify√
  (lambda (v w s)
    (let [(v (walk v s))
          (w (walk w s))]
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
        [else #f]))))
