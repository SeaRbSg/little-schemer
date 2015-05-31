#lang racket
(require "../basic_defs.rkt")
(require "../lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../../lib/mk.rkt")

; # 1 - 8
(define append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))

[check-equal? (append '(a b c) '(d e)) '(a b c d e)]
[check-equal? (append '(a b c) '()) '(a b c)]
[check-equal? (append '() '(d e)) '(d e)]
[check-equal? (append '(d e) 'a) '(d e . a)]

; # 9 & 15
(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (appendo d s res)
         (conso a res out))))))

; # 10 - 14
[check-equal?
  (run* (x) (appendo  '(cake) '(tastes yummy) x))
  '((cake tastes yummy))]

[check-equal?
  (run* (x)
    (fresh (y)
      (appendo
        `(cake with ice ,y)
        '(tastes yummy)
        x)))
  '((cake with ice _.0 tastes yummy))]

[check-equal?
  (run* (x)
    (fresh (y)
      (appendo
        '(cake with ice cream)
        y
        x)))
  '((cake with ice cream . _.0))]

[check-equal?
  (run 1 (x)
    (fresh (y)
      (appendo `(cake with ice . ,y) '(d t) x)))
  '((cake with ice d t))]

; # 16
[check-equal?
  (run 5 (x)
    (fresh (y)
      (appendo `(cake with ice . ,y) '(d t) x)))
  '((cake with ice d t)
    (cake with ice _.0 d t)
    (cake with ice _.0 _.1 d t)
    (cake with ice _.0 _.1 _.2 d t)
    (cake with ice _.0 _.1 _.2 _.3 d t))]

; trivial by now, from the definition of appendo
;     ((nullo l) (== s out))
;     ((fresh (a d res)
;             (conso a d l)           ; y is unfolded into a pair
;             (appendo d s res)       ; in recursion the 1st time hits nullo, the 2nd adds a new car (freshy)
;             (conso a res out))))))  ; consososing as it pulls out

; # 17 - 19
[check-equal?
  (run 5 (y)
    (fresh (x)
      (appendo `(cake with ice . ,y) '(d t) x)))
  '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))]

; we are building the same stuff, but asking a different question (now y instead of x)

; # 20
[check-equal?
  (run 5 (x)
    (fresh (y)
      (appendo
        `(cake with ice . ,y)
        `(d t . ,y)
        x)))
  '((cake with ice d t)
    (cake with ice _.0 d t _.0)
    (cake with ice _.0 _.1 d t _.0 _.1)
    (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
    (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3))]

; # 21 - 22
[check-equal?
  (run* (x)
    (fresh (z)
      (appendo
        '(cake with ice cream)
        `(d t . ,z)
        x)))
  '((cake with ice cream d t . _.0))]

; the key is that z is consososed un-unified, un-unfolded, as-is

; # 23 - 26
[check-equal?
  (run 6 (x)
    (fresh (y)
      (appendo x y '(cake with ice d t))))
  '(() (cake) (cake with) (cake with ice) (cake with ice d) (cake with ice d t))]

[check-equal?
  (run 6 (y)
    (fresh (x)
      (appendo x y '(cake with ice d t))))
  '((cake with ice d t) (with ice d t) (ice d t) (d t) (t) ())]

; # 27 - 28 combining both prefixes and suffixes / cares and coulderes
[check-equal?
  (run 6 (r)
    (fresh (x y)
      (appendo x y '(cake with ice d t))
      (== `(,x ,y) r)))
  '((() (cake with ice d t))
    ((cake) (with ice d t))
    ((cake with) (ice d t))
    ((cake with ice) (d t))
    ((cake with ice d) (t))
    ((cake with ice d t) ()))]

; # 29
;(run 7 (r)
;     (fresh (x y)
;            (appendo x y '(cake with ice d t))
;            (== `(,x ,y) r)))
;
; runs forever because after finding the first 6 sols, it never ends looking for the seventh
; walking the appendo code:
; ...
; (lambda (l s out)
;   (conde
;     ((nullo l) (== s out))
;     ((fresh (a d res)
;             (conso a d l)           ; unfold x into _.0 and _.1
;             (appendo d s res)       ; recurse appendo on _.1, (like with x), keep exiting on nullo
;             (conso a res out))))))  ; but once all the sols are found, nullo is never hit again => infinite loop

; # 30 - 32 How to make it stop on its own?
(define appendauto
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (appendauto d s res)))))) ; recurse at the end

[check-equal?
  (run 7 (r)
    (fresh (x y)
      (appendauto x y '(cake with ice d t))
      (== `(,x ,y) r)))
  '((() (cake with ice d t))
    ((cake) (with ice d t))
    ((cake with) (ice d t))
    ((cake with ice) (d t))
    ((cake with ice d) (t))
    ((cake with ice d t) ()))]

; walkthrough t.b.d

; # 33
[check-equal?
  (run 7 (x)
    (fresh (y z)
      (appendauto x y z)))
  '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3) (_.0 _.1 _.2 _.3 _.4) (_.0 _.1 _.2 _.3 _.4 _.5))]
; (nullo...)                ; first sol is obviously nullo x => ()
; (conso a d x)             ; x is unfolded in pair _0 . _1
; (conso a res z)           ; z is also unfolded in a pair where both heads are equal _0 . _2
; (appendauto d y res)))))) ; appendauto (cdr x) y (cdr z) == app _1 y _2
;                               success at nullo _1 => x is (_0)
;                               recursivelly add another var

; # 34 - 35
[check-equal?
  (run 7 (y)
    (fresh (x z)
      (appendauto x y z)))
  '(_.0 _.0 _.0 _.0 _.0 _.0 _.0)]
; (nullo...)                ; first sol is obviously nullo x => (), s == out, y == z, a freshy _0
; (conso a d x)             ; x is unfolded in pair _0 . _1
; (conso a res z)           ; z is also unfolded in a pair where both heads are equal _0 . _2
; (appendauto d y res)))))) ; appendauto (cdr x) y (cdr z) == app _1 y _2
;                               success at nullo _1 => x is (_0), again y == (cdr z) == another freshy
;                               recursivelly add another var to x, yet again y is always another freshy

; # 36
[check-equal?
  (run 7 (z)
    (fresh (x y)
      (appendauto x y z)))
  '(_.0
     (_.0 . _.1)
     (_.0 _.1 . _.2)
     (_.0 _.1 _.2 . _.3)
     (_.0 _.1 _.2 _.3 . _.4)
     (_.0 _.1 _.2 _.3 _.4 . _.5)
     (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))]
; you can follow the code as we did in the two previous explanations,
; or you can just add the two previous explanations (since appendauting x y gives you z)

; # 37
[check-equal?
  (run 7 (r)
    (fresh (x y z)
      (appendauto x y z)
      (== `(,x ,y ,z) r)))
  '((() _.0 _.0)
    ((_.0) _.1 (_.0 . _.1))
    ((_.0 _.1) _.2 (_.0 _.1 . _.2))
    ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
    ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
    ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
    ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))]

; # 38
(define swappendo
  (lambda (l s out)
    (conde
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (swappendo d s res)))
      ((nullo l) (== s out)))))

; # 39 - 40
; (run 1 (z)
;   (fresh (x y)
;     (swappendo x y z)))
; nooooooooo, it recurs infinitelly and never gets out in the nullo !!!
; swappendo of d s res is swappendo of three fresh variables, like x y z, ad infinitum

; # 41 - 44
(define unpack  ; instead of unwrap
  (lambda (x)
    (cond
      [(pair? x) (unpack (car x))]
      [else x])))

[check-equal? (unpack '((((pizza))))) 'pizza]
[check-equal? (unpack '((((pizza pie) with)) extra cheese)) 'pizza]

; # 45 - 47
(define unpaco
  (lambda (package out)
    (conde
      ((pairo package)
       (fresh (a)
         (caro package a)
         (unpaco a out)))
      ((== package out)))))

[check-equal?
  (run* (x)
    (unpaco '(((pizza))) x))
  '(pizza (pizza) ((pizza)) (((pizza))))]
; the package is unfolded into a pair, and then we have two branches
; the first one recurrs unpacking the car of the package (as long as it can be unfoldd, until we find an atom)
; the second branch always succeeds, it always gives a solution, the package itself at that point

; # 48 - 51
; (run 1 (x) (unpaco x 'pizza))
; it is an infinite loop because it gets down the rabbit hole of recursion and
; never finishes (nor hits the package out conde)
; x => (_0 . _1) => _0 => (_2 . _3) => _2 => (_4 . _5) => ...

; the same applies to:
; (run 1 (x) (unpaco `((,x)) 'pizza))

; # 52
(define unwrapo
  (lambda (package out)
    (conde
      ((== package out))
      ((pairo package)
       (fresh (a)
         (caro package a)
         (unwrapo a out))))))

; # 53
[check-equal?
  (run 5 (x) (unwrapo x 'pizza))
  '(pizza
     (pizza . _.0)
     ((pizza . _.0) . _.1)
     (((pizza . _.0) . _.1) . _.2)
     ((((pizza . _.0) . _.1) . _.2) . _.3))]

; # 54
[check-equal?
  (run 5 (x) (unwrapo x '((pizza))))
  '(((pizza))
    (((pizza)) . _.0)
    ((((pizza)) . _.0) . _.1)
    (((((pizza)) . _.0) . _.1) . _.2)
    ((((((pizza)) . _.0) . _.1) . _.2) . _.3))]

; # 55 - 57
[check-equal?
  (run 5 (x) (unwrapo `((,x)) 'pizza))
  '(pizza
     (pizza . _.0)
     ((pizza . _.0) . _.1)
     (((pizza . _.0) . _.1) . _.2)
     ((((pizza . _.0) . _.1) . _.2) . _.3))]
; unwrapo ((x)) pizza
;   -c1 ((x)) =/= pizza ❌
;   -c2 ((x)) == ((x) . _.0)
;       unwrapo (x) pizza
;         -c1 (x) =/= pizza ❌
;         -c2 (x) == (x . _.0) 
;             unwrapo x pizza
;               -c1 x == pizza ✅  [SOL 1]
;               -c2 x == (_.0 . _.1)
;                   unwrapo _.0 pizza
;                     -c1 _.0 == pizza
;                         x == (pizza . _.1) ✅  [SOL 2]
;                     -c2 x == ((_.2 . _.3) . _.1)
;                         unwrapo _.2 pizza
;                           -c1 _.2 == pizza
;                               x == ((pizza . _3) . _1) ✅  [SOL 3]
;                           -c2 etc…

; # 58
(define flatten
  (lambda (s)
    (cond
      [(null? s) '()]
      [(pair? s)
       (append
         (flatten (car s))
         (flatten (cdr s)))]
      [else (cons s '())])))

[check-equal? (flatten '((a b) c)) '(a b c)]

; # 59 - 61 (my definition)
(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== s out))
      ((fresh (a d aa dd)
         (conso a d s)
         (flatteno a aa)
         (flatteno d dd)
         (appendo aa dd out)))
      ((conso s '() out)))))

[check-equal? (run 1 (x) (flatteno '((a b) c) x)) '((a b c))]
[check-equal? (run 1 (x) (flatteno '(a (b c)) x)) '((a b c))]

; # 62 - 63 see drawing ch25_62.png for walkthrough
[check-equal?
  (run 3 (x)
    (flatteno '(a) x))
  '((a) (a ()) ((a)))]

; # 64 - 65 see drawing ch25_64.png for walkthrough
[check-equal?
  (run* (x)
    (flatteno '((a)) x))
  '((a) (a ()) (a ()) (a () ()) ((a)) ((a) ()) (((a))))]

; # 66 - 67 see ch25_66.png
[check-equal?
  (run* (x)
    (flatteno '(((a))) x))
  '((a) (a ( ))
        (a ( )) (a ( ) ( ))
        (a ( )) (a ( ) ( ))
        (a () ()) (a () () ())
        ((a)) ((a) ())
        ((a) ()) ((a) () ())
        (((a))) (((a)) ())
        ((((a)))))]

; # 68 - 70 see ch25_68.png
[check-equal?
  (run* (x)
    (flatteno '((a b) c) x))
  '((a b c) (a b c ()) (a b (c))
            (a b () c) (a b () c ()) (a b () (c))
            (a (b) c) (a (b) c ()) (a (b) (c))
            ((a b) c) ((a b) c ()) ((a b) (c))
            (((a b) c)))]

; # 71 - 76
(define flatuleno
  (lambda (s out)
    (conde
      ((conso s '() out))
      ((nullo s) (== s out))
      ((fresh (a d aa dd)
         (conso a d s)
         (flatuleno a aa)
         (flatuleno d dd)
         (appendo aa dd out))))))

; I dont think the pairo goal was needed before either, conso suffices (imo)
; see ch25_75.png
[check-equal?
  (run* (x)
    (flatuleno '((a b) c) x))
  '((((a b) c))
    ((a b) (c)) ((a b) c ()) ((a b) c)
    (a (b) (c)) (a (b) c ()) (a (b) c)
    (a b () (c)) (a b () c ()) (a b () c)
    (a b (c)) (a b c ()) (a b c))]

; # 77
[check-equal?
  (run 1 (x)
    (flatuleno x '(a b c)))
  '((a b . c))]

; this one is a dozy, see my quasy explanation on 77.rkt
