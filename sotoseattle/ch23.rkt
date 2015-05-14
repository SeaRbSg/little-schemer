#lang racket
(require "./basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

[check-equal?
 (run* (x)
       (listo `(a b ,x d)))
 '(_.0)]
 
[check-equal?
 (run 1 (x)
      (listo (cons 'a (cons 'b x )))) ; ('a 'b . x)
 '(())]

[check-equal?
 (run 5 (x)
      (listo (cons 'a x)))          ; ('a . x)
 '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))]

; FIRST APPROACH: pure logic
; he x that makes the listo succeed can be any possible list (including the empty one).

; SECOND APPROACH: walk the code
; Keep in mind that: (run* (q) (pairo q)) '(_.0 . _.1) is a solution for q, and that 
; (run* (x) (nullo x)) '() is a sol for x

; We start with l being ('a . x)
;   [nullo ?] Nope, it is not unificable with '(), we go to next conde
;    [pairo ?] Yeap, it is a pairo (because it can be broken in car and cdr)
;      we bind x with d
;      we do listo of x ==> Recurr ==> We start with l being x
;        [nullo ?] YES, succeeds because (nullo x) '() is sol of x => FIRST SOL '()
;        [pairo ?] YES, succeeds because (pairo x) '(_.0 . _.1) is sol for x
;          we bind _.1 to d
;          we do listo of _.1 ==> Recurr == We start with l being _.1
;            [nullo ?] YES, get out of recursion with _.1 == '() ==> SECOND SOL '(_.0)
;            [pairo ?] YES, '(_.2 . _.3) is sol of _.1
;              we bind _.3 to d
;              we do listo of _.3 ==> Recurr == We start with l being _.3
;                ... ad infinitum and beyond ...

(define lol?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(list? (car l)) (lol? (cdr l))]
      [else #f])))

(define lolo
  (lambda (l)
    (conde
     ((nullo l) s#)
     ((fresh (a)
             (caro l a)
             (listo a))
      (fresh (b)
             (cdro l b)
             (lolo b)))
     (else u#))))

[check-equal?
 (run 1 (l) (lolo l))
 '(())]

[check-equal?
 (run* (q)
       (fresh (x y)
              (lolo `((a b) (,x c) (d ,y)))
              (== #t q)))
 '(#t)]

[check-equal?
 (run 1 (q)
      (fresh (x)
             (lolo `((a b) . ,x))
             (== #t q)))
 '(#t)]

; for the same reasons as before in the long explanation. x nullo succeeds and is the first conde

[check-equal?
 (run 5 (q)
      (fresh (x)
             (lolo `((a) . ,x))
             (== #t q)))
 '(#t #t #t #t #t)]

; Why the above infinites if every solution is #t ????

; keep in mind that:
[check-equal? (run* (x) (fresh (k) (caro k x))) '(_.0)]
[check-equal? (run* (x) (fresh (k) (cdro k x))) '(_.0)]

; let's walk the lolo code again:
; l is (('a) . x)
;   [nullo ?] nope
;   [caro] extract a as '(a), is '(a) a listo? YES, but go to next
;   [cdro] extract b as x, is x a lolo?, let's see:
;     l is x
;       [nullo ?] YES, x is '() => that overall lolo succeeds => q is #t
;       [caro] a is a freshy _.0, wich is a listo (all possible listos), let's see the second condition
;       [cdro] b is a freshy , is it a lolo?
;         l is '(_.0)
;           [nullo ?] Yes, (nullo of freshy) succeeds for freshy being '() => AGAIN SOLUTION #t
;           [caro] ... repeats as before because is just another freshy ...
; so ad infinitum we find hits solutions that succeed (as they hit the nullos)...
; ... which means an infinite stream of #t

