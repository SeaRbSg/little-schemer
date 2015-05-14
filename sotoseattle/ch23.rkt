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
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (listo a))
      (fresh (b)
             (cdro l b)
             (lolo b))))))

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
 (run 3 (q)
      (lolo `((a) . ,q)))
 '(() (()) (() ()))]

; keep in mind:
; (run* (x) (fresh (k) (caro (cons '(a) x) k)))
; (run* (x) (fresh (k) (caro x k)))
; (run* (x) (fresh (k) (cdro x k)))

; let's walk the lolo code again:
; l is (('a) . x)
;   [nullo ?] nope
;   [caro] extract a as '(a), is it a listo? YES, but go to next
;   [cdro] extract b as x, is x a lolo?, let's see:
;     l is x, we get in conde (OR conditions)
;       Conde 1: [nullo] YES, x is '() => that overall lolo succeeds => first solution of q is '()
;       Conde 2: [caro] a is a freshy _.0, wich is a listo (all possible listos), &&
;                [cdro] b is a freshy , is it a lolo?
;                l is '(_.0), we get in conde (OR conditions)
;                  Conde 1: [nullo] Yes, (nullo of freshy) succeeds for freshy being '() => 2nd solution is '(())
;                     Of all the possible listos of before, this one collapses the solutions domain 
;                     to make both goals succeed
;                  Conde 2: [caro] yes
;                           [cdro] b is a freshy again ==> (run* (x) (fresh (k) (cdro x k))) => (_.0 . _.1)
;                           l is '(_.0), we get in conde (OR conditions)
;                             Conde 1: [nullo] Yes, pull out, again of all possible listos => '(() ())
; so ad infinitum we find hits that succeed and add '() to the list of solutions

(define twinso_0
  (lambda (s)
    (fresh (x y)
           (conso x y s)
           (conso x '() y))))

[check-equal?
 (run* (q)
       (twinso_0 '(tofu tofu))
       (== #t q))
 '(#t)]

[check-equal?
 (run* (z)
       (twinso_0 `(,z tofu)))
 '(tofu)]

(define twinso
  (lambda (s)
    (fresh (x y)
           (== `(,x ,x) s)))) ; <==== coool

[check-equal?
 (run* (z)
       (twinso `(,z tofu)))
 '(tofu)]

(define loto
  (lambda (l)
    (conde
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (twinso a))
      (fresh (b)
             (cdro l b)
             (loto b))))))

[check-equal?
 (run 1 (q) (loto q))
 '(())]                ; obvious, the nullo succeeds for empty list

[check-equal?
 (run 2 (q) (loto q))
 '(() ((_.0 _.0)))]

; caro of q => (_.0 . _.1) && twinso means sol is (_.0 _.0) << now proper >>
; (run* (q) (fresh (x) (caro q x) (twinso q)))
; so so far, l has a first element with (_.0 _.0), we do not know the second ((_.0 _.0) . ??)

; cdro of q => (_.0 . _.1) and loto of that means that the first conde it finds is the nullo
; but the (nullo (_.0 . _.1)) fails with '() => pulls out into ((_.0 _.0) . ??):
; ((_.0 _.0) . ()) ==> ((_.0 _.0)) <====== OUR SECOND SOLUTION
; 
; since it was a conde (OR conds), lets go back to the car/twinso && cdr/loto of (_.0 . _.1)
; the caro/twinso is clear again ==> (fresh_i . fresh_j) == twinsoed into (fresh_i fresh_i)
; the cdro works similarly, and the recurring lotto will fail again in the nullo with '(), so
; ((_.0 _.0) . ??) ==> ((_.0 _.0) . ((fresh_i fresh_i)  '())) =>
; ((_.0 _.0) . ((fresh_i fresh_i))) ==> ((_.0 _.0) (fresh_i fresh_i)) ==>
; ((_.0 _.0) (_.1 _.1)) ==> OUR THIRD SOLUTION, and on and on

[check-equal?
 (run 1 (z)
      (loto `((g g) . ,z)))
 '(())]

[check-equal?
 (run 3 (out)
      (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (loto out)))
 '(((g g) (e e) (_.0 _.0)) 
   ((g g) (e e) (_.0 _.0) (_.1 _.1)) 
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]

(define listofo
  (lambda (predo l)
    (conde
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))))))

[check-equal?
 (run 3 (out)
      (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (listofo twinso out)))
 '(((g g) (e e) (_.0 _.0)) 
   ((g g) (e e) (_.0 _.0) (_.1 _.1)) 
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]

; we are just passing the twinso as argument predo

(define lototo
  (lambda (l)
    (listofo twinso l)))

; ------------------------------------------------------------

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) #t]
      [else (member? x (cdr l))])))

[check-true (member? 'olive '(virgin olive oil))]

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
     ;((nullo? l) u#)           ; unnecessary like the else statements because it fails
     ((eq-caro l x))
     ((fresh (b)                ; else is unnecessary because inside a specific conde all goals are &&
             (cdro l b)
             (membero x b))))))

[check-equal?
 (run* (q)
       (membero 'olive '(virgin olive oil))
       (== #t q))
 '(#t)]

[check-equal? (run* (y) (membero y '())) '()] ; because (caro/cdro '() x) both fail
                                              ; (run* (x) (caro '() x) (== x #t))

[check-equal? (run 1 (y) (membero y '(hummus with pita))) '(hummus)]
[check-equal? (run* (y) (membero y '(a b c))) '(a b c)]

; membero gives back a list of solutions where every solution satisfies that it is a member of the given list
; the caro conde finds solutions of x
; the cdro conde, being a different conde (OR), refreshes and as it recurrs finds NEW solutions of x
;   that are added to the list of solution ==> becoming in order, the initial l

(define identity
  (lambda (l)
    (run* (q)
          (membero q l))))

[check-equal? 
 (run* (x)
       (membero 'e `(pasta ,x fagioli)))
 '(e)]

; interestingly enough a solution for x is 'e because it will succeed only at the eq-caro '(,x) 'e
; and caro is about unifying, about checking if x and 'e can be unified,
; independently of the order (== x 'e) === (== 'e x)

; for the following thread slowly...

[check-equal?
 (run 1 (x) (membero 'e `(e)))    ; ok, logical. membero succeeds but x is unknown
 '(_.0)]

[check-equal?
 (run 1 (x) (membero 'e `(,x)))   ; ok, logical too, now x has a solution
 '(e)]

[check-equal?
 (run* (x) (membero 'e `(e ,x)))  ; now it makes sense
 '(_.0 e)]

[check-equal?
 (run* (x) (membero 'e `(pasta ,x e fartura)))  ; got it
 '(e _.0)]

[check-equal?
 (run* (r)
       (fresh (x y)
              (membero 'e `(pasta ,x fagioli ,y))
              (== `(,x ,y) r)))
 '((e _.0) (_.0 e))]

; the important thing is that as we change condes, as we have the caro succeed, the x is refreshed!

(run 4 (l)
     (membero 'tofu l))