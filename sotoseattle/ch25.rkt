#lang racket
(require "./basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

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

; # 34
[check-equal?
 (run 7 (y)
      (fresh (x z)
             (appendauto x y z)))
 '(_.0 _.0 _.0 _.0 _.0 _.0 _.0)]

