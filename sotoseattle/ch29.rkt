#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

(define var  (lambda (freshy) (vector freshy)))
(define var? (lambda (anything) (vector? anything)))
(define lhs  (lambda (ass) (car ass)))
(define rhs  (lambda (ass) (cdr ass)))

; we can define our own fresh variables!!!!
(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

; (z . a) is a malformed list of two atoms
;         or an association between a variable [z] and a value [a]
;         for which z must be a var and a can be anything

; An assoc cannot be self-referencing (x . x) because it would always
; imply an infinite loop (when looking for the val assoc with x

; (rsh assoc => value
; (lhs assoc => key [var]

; A substitution is a list of assocs

(define empty-s '()) ; empty substitution, a subst with no assocs inside

; (walk z '((z . a) (x . w) (y . z))) ; z ==> a (end of walk)
; (walk y '((z . a) (x . w) (y . z))) ; y ==> z ~> z ==> a (end of walk)
; (walk x '((z . a) (x . w) (y . z))) ; x ==> w (end of walk)
; (walk w '((z . a) (x . w) (y . z))) ; w ==> nothing => w (end of walk == start of walk)
; (walk x '((x . y) (z . x) (y . z))) ; x => y => z => x (infinite loop) => no value

; # 21 <==== DO NOT UNDERSTAND

; # 27
(define walk_0
  (lambda (v s)
    (cond
      [(var? v) (cond
                  [(assq v s) => (lambda (a) (walk_0 (rhs a) s))]
                  [else v])]
      [else v])))

; The else of the first cond is easy. Given a key and a substitution, if the key
; is not even a var, don't get tired walking, just return it.
; If it is a var, search (assq) and if it doesn't find it, return itself (inside else),
; if it finds it, recur the walk but now using as key the rhs value found.
; This will stop when there is nothing else to find, and trigger the inside else.

; # 27 bis -- the following is longer but easier to follow

(define wander
  (lambda (ass s)
    (walk (rhs ass) s)))

(define walk
  (lambda (v s)
    (cond
      [(var? v) (let ((ass_found (assq v s)))
                  (cond
                    [ass_found (wander ass_found s)]
                    [else v]))]
      [else v])))

[check-equal? (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a]
[check-equal? (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a]
[check-equal? (walk x `((,z . a) (,x . ,w) (,y . ,z)))  w]
[check-equal? (walk w `((,z . a) (,x . ,w) (,y . ,z)))  w]

; # 29
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

; (walk x (ext-s x y `((,z . ,x) (,y . ,z)))) <=== moebius path
[check-equal? (walk y (ext-s x 'pepe `((,z . ,x) (,y . ,z)))) 'pepe]

; # 35 I DO NOT UNDERSTAND <=========

(define unify
  (lambda (v w s)
    (let ((v (walk v s))           ; don't waste time, v is its end of the line
          (w (walk w s)))          ; the same for w
      (cond
        [(eq? v w) s]              ; if v & w are the exact same thing => don't extend to (x . x)!!
        [(var? v) (ext-s v w s)]
        [(var? w) (ext-s w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify (car v) (car w) s) => (lambda (s) (unify (cdr v) (cdr w) s))] ; [see below]
           [else #f])]
        [(equal? v w) s]           ; not being the same thing, yet identical
        [else #f]))))

; (something) => (lambda (whatever) ...)
; if something is truthy (and returns a whatever), then call the lambda with
; that whatever returned by the something call
; in any other case something will be #f and jump out

; we know that unify returns #f or a substitution
; it unifies recurring from left to right and top to bottom (like * procs)
; in case of mismatch of unify fail => it breaks and jumps out with a #f

; # 44
[check-equal? (walk x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a ,z c)]
; this is a superficial walk
; walk* would try to find the eol of the freshy returned in the solution (a a c)

[check-equal? (walk x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(,z ,w c)]
; walk* would try to find the eol of the freshy returned in the solution (a ,w c)

; (define walk_0
;   (lambda (v s)
;     (cond
;       [(var? v) (cond
;                   [(assq v s) => (lambda (a) (walk_0 (rhs a) s))]
;                   [else v])]
;       [else v])))

(define my_walk*
  (lambda (v s)
    (cond
      [(var? v) (cond
                  [(assq v s) => (lambda (a) (my_walk* (rhs a) s))]
                  [else v])]
      [(pair? v) (cons (my_walk* (car v) s) (my_walk* (cdr v) s))]
      [else v])))

[check-equal? (my_walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a a c)]
[check-equal? (my_walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(a ,w c)]

; we can simplify reusing walk
(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) v]
        [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
        [else v]))))

[check-equal? (walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a a c)]
[check-equal? (walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(a ,w c)]

; A VARIABLE THAT HAS BEEN WALKED AND ENDS BEING A VAR ==> IS FRESH ALWAYS!!!
; THINK THINK THINK

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

[check-equal? (reify-name 67535) '_.67535]

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) (ext-s v (reify-name (length s)) s)]    ; length of s as we insert
        [(pair? v) (reify-s (cdr v) (reify-s (car v) s))] ; DONT UNDERSTAND!!
        [else s]))))

[check-equal?
  (reify-s x empty-s) ; x is var => insert (x . _.0) into empty-s ()
  `((,x . _.0))]

[check-equal?
  (reify-s (list x y) empty-s) ; now pair => same but from bottom up => first y then x
  `((,y . _.1) (,x . _.0))]

[check-equal?
  (reify-s (list w x y) empty-s) ; check the numbering !!
  `((,y . _.2) (,x . _.1) (,w . _.0))]

[check-equal?
  (let ((r (list w x y)))
    (walk* r (reify-s r empty-s))) ; (walk (w x y) ((y . _.2) (x . _.1) (w . _.0)))
  '(_.0 _.1 _.2)]

[check-equal?
  (let ((r (walk* (list x y z) empty-s))) ; (x y z) HA HA, its the same
    (walk* r (reify-s r empty-s)))
  '(_.0 _.1 _.2)]

[check-equal?
  (let ((r `(,u (,v (,w ,x) ,y) ,x))) ; x is repeated and already reified
    (walk* r (reify-s r empty-s)))    ; _0 _1 _2 _3 _4 _3
  '(_.0 (_.1 (_.2 _.3) _.4) _.3)]     ; THINK AGAIN THE NUMBERING

[check-equal?
  (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
    (let ((r (walk* x s)))                             ; a ,w c ,w
      (walk* r (reify-s r empty-s))))                  ; a _.0 c _.0
  '(a _.0 c _.0)]

