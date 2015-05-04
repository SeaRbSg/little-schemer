#lang racket
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require minikanren)

(define s# (== #f #f))
(define u# (== #f #t))

[check-equal?
 (run* (q)
       u#
       (== #t q))
 '()]

; two goals, and the first #fails at u# => look no more => q is '()

[check-equal?
 (run* (q)
       (== #t q)
       u#)
 '()]

; it #succeeds at first, but the second goal #fails at u# => don't think => q is '()
; the goals have to succeed like in an AND condition
; goals have the commutative property. The order is not important

[check-equal?
 (run* (q)
       s#
       (== #t q))
 '(#t)]

; after the first goal succeeds, q is a fresh variable (not '()), 
; after the second goal, that fresh variable is associated with #t

; ----------------------------------------------------------------------

; we can use goals like (== x y),
; or we can use the result of goals like u#, s# (which work as goals themselves)
; or we can use (let ) expressions, which will be logically successful or not

[check-equal?
 (run* (x)
       (let ((x #t)) (== #f x))) ; goal fails => ()
 '()]

[check-equal?
 (run* (x)
       (let ((x #f)) (== #f x))) ; goal succeeds => fresh_x
 '(_.0)]

; A fresh variable is a virginal variable. Not associated yet with anything.
; Law of Fresh
; a (== something freshy) succeeds always, since freshy doesn't know better and always says yes to something.

; But once freshy knows something, innocence is lost and cannot not know never again :(

[check-equal?
 (run* (q)
       (== q #t)   ; knows the truth
       (== q #f))  ; tries to unkonw the truth and change mind => boom ! goes crazy and fails => q is () empty
 '()]

[check-equal?
 (run* (q)
       (== q #t)   ; knows the truth
       (== q #t))  ; that is ok, the truth doesn't change, sanity is preserved
 '(#t)]

[check-equal?
 (run* (q) s#) ; since it succeeds q cannot be (), but it has no association yet (value), so it gives back q as a freshy
 '(_.0)]

; ----------------------------------------------------------------------

[check-equal?
 (run* (x)
       (let ((x #f))
         (fresh (x)
                (== #t x))))
 '(_.0)]

; is the same as...

[check-equal?
 (run* (x)
       (let ((x1 #f))
         (fresh (x2)
                (== #t x2))))
 '(_.0)]

; starting at the bottom:
; (fresh (x2) (== #t x2)) succeeds
; (let ((x1 #f)) << x2 == #t >>) succeeds too
; so x cannot be (), but it has no association, therefore => _.0 freshy

; ----------------------------------------------------------------------

[check-equal?
 (run* (r)
       (fresh (x y)
              (== (cons x (cons y '())) r)))
 '((_.0 _.1))]

; a cute way to mitosis, instead of a freshy_r we get two in a list (freshy_x freshy_y)
; as with Scheme prev books, dont get hang up on variable names

; ----------------------------------------------------------------------

[check-equal?
 (run* (r)
       (fresh (x)
              (let ((y x))
                (fresh (x)
                       (== (cons y (cons x (cons y '()))) r)))))
 '((_.0 _.1 _.0))]

; is the same as

[check-equal?
 (run* (r)
       (fresh (x1)
              (let ((y x1))
                (fresh (x2)
                       (== (cons y (cons x2 (cons y '()))) r)))))
 '((_.0 _.1 _.0))]

; (y x2 y) == (x1 x2 x1) == r == (_0 _1 _0)

; ----------------------------------------------------------------------

; read slowly, what is the VALUE?
[check-equal?
 (cond [#f 'whatever] [else #f])  ; => the first condition blows, the else returns that #f <== value
 #f]

; read slowly, does it SUCCEED?
;(cond [#f 'dont_care] [else u#]) ; => #f blows, the else hits the unsucceed!

; conde is different from cond, it means cons all successful conditions (every cond)

[check-equal?
 (run* (x)
       (conde
        [(== 'olive x) s#]  ; x was fresh, associated to olive succeeds => success!! (store)
        [(== 'oil x) s#]    ; in the new cond x is REFRESHED, associated with oil => success!! (store)
        [u#]))              ; no more conds (this is the else) => x was associated with olive and oil
                            ; when x returns it brings back both assocs in a list (olive oil)
 '(olive oil)]

; (run1 (x)...) is the same as run*, but get just the first element of x

[check-equal?
 (run* (x)
       (conde
        [(== 'virgin x) u#] ; x was fresh, associated to virgin succeeds => anwers is fail! => (throw away)
        [(== 'olive  x) s#] ; x refreshed, associated to olive succeeds => success!! (store)
        [s# s#]             ; x refreshed, s# succeeds => answer success => store (but without association)
        [(== 'oil x)    s#] ; x refreshed, associated to oil succeeds => success!! (store)
        [u#]))
 '(olive _.0 oil)]

; x is then (olive _0 oil)
; conde works as a bunch of OR conditions, only pick the ones that succeed
; NOW, if all of them fail, and that means ALL => unsucceed and x is ()

[check-equal?
 (run* (x)
       (conde
        [(== 'virgin x) u#] ; throw away
        [(== 'olive  x) u#] ; throw away
        [s# s#]             ; x refreshed, s# succeeds => answer success => store (but without association)
        [u# s#]             ; fails, throw away
        [(== 'oil x)    u#] ; throw away
        [u#]))
 '(_.0)]

; x is (_0)

[check-equal?
 (run* (x)
       (conde
        [(== 'virgin x) u#] ; throw away
        [(== 'olive  x) u#] ; throw away
        [u# s#]             ; throw away
        [(== 'oil x)    u#] ; throw away
        [u#]))
 '()]

; x is ()

; QUESTION: Why do we need an ELSE?
; if all conds fail, and x cannot return even a fresh variable, the only option is an empty list (no?)

[check-equal?
 (run* (x)
       (conde
        [(== 'virgin x) u#] ; throw away
        [(== 'olive  x) u#] ; throw away
        [u# s#]             ; throw away
        [(== 'oil x)    u#] ; throw away
        ))
 '()]

[check-equal?
 (run* (r)
       (fresh (x y)
              (conde
               [(== 'split x) (== 'pea y)]   ; succeeds both Q and A => store (split pea)
               [(== 'navy x) (== 'beans y)]  ; succeeds both Q and A => store (navy beans)
               [u#])
              (== (cons x (cons y '(soup))) r))) ; r == (x y) but we have 2! => ((split pea soup) (navy beans soup))
 '((split pea soup) (navy beans soup))]

; ----------------------------------------------------------------------

(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) s#]
     [(== 'cup x) s#]
     [u#])))

[check-equal? (run* (x) (teacupo x)) '(tea cup)]

[check-equal?
 (run* (r)
       (fresh (x y)
              (conde
               [(teacupo x) (== #t y) s#]    ; x -> (tea cup), y -> #t => combinations [(tea t) (cup t)]
               [(== #f x) (== #t y) s#]      ; x -> #f, y-> #t
               ; [u#] ; from now on I delete all these ELSEs
               )
              (== (cons x (cons y '())) r)))
 '((#f #t) (tea #t) (cup #t))]

; QUESTION: What is (teacupo 'pepe)? A procedure, a function that plays with goals ??

; ----------------------------------------------------------------------

[check-equal?
 (run* (r)
       (fresh (x y z)
              (conde
               [(== y x) (fresh (x) (== z x))]
               [(fresh (x) (== y x)) (== z x)])
              (== (cons y (cons z '())) r)))
 '((_.0 _.1) (_.0 _.1))]
           
; The same as

[check-equal?
 (run* (r)
       (fresh (x y z)
              (conde
               [(== y x) (fresh (k) (== z k))]  ; x == y && z is fresh => (y z) => (x k) => (_0 _1) two fresh vars, in order 0 & 1
               [(fresh (j) (== y j)) (== z x)]) ; z == x && y is fresh => (y z) => (j x) => (_0 _1) two fresh vars, in order 0 & 1
              (== (cons y (cons z '())) r)))    ; r => (y z) => ((_0 _1) (_0 _1))
 '((_.0 _.1) (_.0 _.1))]

; The key concept is that _0 and _1 are different in each (_0 _1) of the prev run
; I think that as we refresh we put all counters to 0, everything is forgotten, so we start assigning freshy variables
; from scratch. In this way ((_0 _1) (_0 _1)) is the same as ((_0 _1) (_2 _3)), we just don't do that because we actually 
; need to freshies to make it all work (again because they refresh and although _0 != _0.refreshed, you only need one _0) ????

[check-equal?
 (run* (r)
       (fresh (x y z)
              (conde
               [(== y x) (fresh (x) (== z x))]
               [(fresh (x) (== y x)) (== z x)])
              (== #f x)
              (== (cons y (cons z '())) r)))
 '((#f _.0) (_.0 #f))]

; r => (y z) => ((x z) (y x)) => ((x k) (j x)) => << x is #f >> => ((#f k) (j #f)) => ((#f _1) (_0 #f)) => ((#f _0) (_0 #f))
; again, they are different runs so although we really have _1 and _0, we only need one since they refresh, so we can
; use the same _0 as in ((#f _0) (_0 #f))

; ----------------------------------------------------------------------

[check-equal?
 (run* (q)
       (let ((a (== #f q))
             (b (== #t q)))
         b))
 '(#t)]

; (== x y) is an expression, its value (assigned to a or b) is a goal (again, wtf is a goal?)
; it is not like lazy assignment, where q is #t or #f depending on which a or b is evaluated
; because below both are called

[check-equal?
 (run* (q)
       (let ((a (== 'luis q))
             (b (== 'pepe q)))
         a
         b))
 '(pepe)]

; maybe it is more like a tree, you walk down a path so the other paths dont count, ummmmm I wonder...
