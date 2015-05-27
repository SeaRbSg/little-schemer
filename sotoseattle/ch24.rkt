#lang racket
(require "./basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

; # 1 - 6

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define mem
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

[check-equal? (mem 'tofu '(a b peas d peas e)) #f]

[check-equal?
 (run* (out)
       (== (mem 'tofu '(a b tofu d peas e)) out))
 '((tofu d peas e))]

[check-equal?
 (mem 'peas
      (mem 'tofu '(a b tofu d peas e)))
 '(peas e)]

[check-equal?
 (mem 'tofu
      (mem 'tofu '(a b tofu d tofu e)))
 '(tofu d tofu e)]

[check-equal?
 (mem 'tofu
      (cdr (mem 'tofu '(a b tofu d tofu e))))
 '(tofu e)]

; # 7
(define eq-caro
  (lambda (l x)
    (caro l x)))

(define memo
  (lambda (x l out)
    (conde
     ((eq-caro l x) (== l out))
     ((fresh (d)
             (cdro l d)
             (memo x d out))))))

; how does is this different from listo, lolailo and membrillo?
; # 10
[check-equal?
 (run 1 (out)
      (memo 'tofu '(a b tofu d tofu e) out))
 '((tofu d tofu e))]

; # 11
[check-equal?
 (run 1 (out)
      (fresh (x)
             (memo 'tofu `(a b ,x d tofu e) out)))
 '((tofu d tofu e))]

; # 12
[check-equal?
 (run* (x)
       (memo x '(a b tofu d tofu e) '(tofu d tofu e)))
 '(tofu)]

; [conde 1] (eq-caro '(a b tofu d...) x) => x == a && '(a b tofu d..) =/= '(tofu d..) FAILS
; [conde 2] (memo x '(b tofu d..) '(tofu d..))
;   [conde 1] for the same reason fails
;   [conde 2] (memo x '(tofu d..) '(tofu d..))
;     [conde 1] YES x == tofu && '(tofu d..) == '(tofu d..) <== SOL #1 tofu
;     [conde 2] (memo x '(d tofu e) '(tofu d tofu e)
;       [conde 1] will always fail in the seconde condition      \__\ No more solutions
;       [conde 2] will keep iterating until cdro is extinguished /  /

; # 13 - 16
[check-equal?
 (run* (q)
       (memo 'tofu '(tofu e) '(tofu e)) ; for the same reasons as above it succeeds once
       (== #t q))
 '(#t)]

[check-equal?
 (run* (q)
       (memo 'tofu '(tofu e) '(tofu))
       (== #t q))
 '()]

[check-equal?
 (run* (x)
       (memo 'tofu '(tofu e) `(,x e)))
 '(tofu)]

[check-equal?
 (run* (x)
       (memo 'tofu '(tofu e) `(peas ,x))) ; no fucking way to '(tofu e) == `(peas ...)
 '()]

; # 17
[check-equal?
 (run* (out)
       (fresh (x)
              (memo 'tofu `(a b ,x d tofu e) out)))
 '((tofu d tofu e) (tofu e))]

; when it reaches the ,x
; [conde 1] x == 'tofu and the second cond succeeds => SOL #1 '(tofu d tofu e)
; [conde 2] (memo of cdro) ... until the next tofu => SOL #2 '(tofu e)

; # 18 - 20
; let's start with the core of the exercise
[check-equal?
 (run 4 (z)
      (fresh (u)
             (memo 'tofu z u)))
 '((tofu . _.0) (_.0 tofu . _.1) (_.0 _.1 tofu . _.2) (_.0 _.1 _.2 tofu . _.3))]

; [conde 1] ((eq-caro l 'tofu) (== l u))
; [conde 2] ((fresh (d) (cdro l d) (memo 'tofu d u))))))

; [c1] caro makes z '(_.i . _.j) where _.i is 'tofu, the second cond makes z == u => SOL #1 '(tofu . _.0)
; [c2] forget about c1, cdro again makes z '(_.i . _.j), call to (memo 'tofu '(_.j) u)
;   [c1] caro unfolds l into two pieces '(_.j) == '(_.0 . _.1)
;        _.0 becomes 'tofu => Succeeds => goes out as (_.i . (tofu . _.1)) => '(_.0 tofu . _.1) SOL#2
;   [c2] cdro unfolds l into two pieces '(_.j) == '(_.0 . _.1)
;        calls again (memo 'tofu '(_.1) u), which is the same as before so:
;        we are building a sol of type (_.0 _.1 . (_.2 . _.3))
;        the (_.2 . _.3) is the one now that will succeed in the caro => (tofu . _.3)
;        adding it all up => SOL #4 (_.0 _.1 tofu . _.3)
;        ... everything follows the same logic

[check-equal?
 (run 12 (z)
      (fresh (u)
             (memo 'tofu `(a b tofu d tofu e . ,z) u)))
 '(_.0
   _.0
   (tofu . _.0)
   (_.0 tofu . _.1)
   (_.0 _.1 tofu . _.2)
   (_.0 _.1 _.2 tofu . _.3)
   (_.0 _.1 _.2 _.3 tofu . _.4)
   (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
   (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9))]

; consider that (== l out) will always succeed
; and that the second conde is just a recursion over the cdro of l

; fast forward to when it reaches the first tofu
; [conde 1] 'tofu == 'tofu && '(tofu d tofu e . _.0) == u ==> OK SOL #1 _.0
; [conde 2] get down ... to the second tofu
;   [conde 1] car ok && '(tofu e . _.0) == u ==> OK SOL #2 _.0
;   [conde 2] (memo 'tofu '(e . ,z) u) ... drill down till z
;   we have seen this in the previous code

; # 22 - 26
(define rember
  (lambda (x l)
    (cond
      [(null? l) '()]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l) (rember x (cdr l)))])))

[check-equal? (rember 'peas '(a b peas d peas e)) '(a b d peas e)]

(define rumbero_v1
  (lambda (x l out)
    (conde
     ((nullo l) (== out '()))
     ((eq-caro l x) (cdro l out))
     ((fresh (result head tail)
            (cdro l tail)
            (rumbero x tail result)
            (caro l head)
            (conso head result out))))))

; # 27 - 29
(define rumbero
  (lambda (x l out)
    (conde
     ((nullo l) (== out '()))
     ((eq-caro l x) (cdro l out))
     ((fresh (result head tail)
            (conso head tail l)          ; the same as (caro l head) && (cdro l tail)
            (rumbero x tail result)      ; as it identifies/binds it separates
            (conso head result out)))))) ; as it identifies/binds it joins

; # 30 - 48
[check-equal?
  (run 1 (out)
    (fresh (y)
      (rumbero 'peas `(a b ,y d peas e) out)))
  '((a b d peas e))]

[check-equal?
 (run* (out)
       (fresh (y z)
              (rumbero y `(a b ,y d ,z e) out)))
 '((b a d _.0 e)
   (a b d _.0 e)
   (a b d _.0 e)
   (a b d _.0 e)
   (a b _.0 d e)
   (a b e d _.0)
   (a b _.0 d _.1 e))]

; it is much simpler to use logic. All answers are easy.

; # 49 - 56

[check-equal?
 (run* (r)
       (fresh (y z)
              (rumbero y `(,y d ,z e) `(,y d e))
              (== `(,y ,z) r)))
 '((d d) (d d) (_.0 _.0) (e e))]

; # 57 - 67

[check-equal?
 (run 13 (w)
      (fresh (y z out)
             (rumbero y `(a b ,y d ,z . ,w) out)))
 '(_.0                      ; y is a, the simplest w is ()
  _.0                       ; y is b, the simplest w is also ()
  _.0                       ; y is y, the simplest w is also ()
  _.0                       ; y is d, the simplest w is also ()
  _.0                       ; y is z (co refer), the simplest w is also ()
  ()                        ; see bellow
  (_.0 . _.1)
  (_.0)
  (_.0 _.1 . _.2)
  (_.0 _.1)
  (_.0 _.1 _.2 . _.3)
  (_.0 _.1 _.2)
  (_.0 _.1 _.2 _.3 . _.4))]

; once we reach w, lets do (rumbero y `(,w) out), where w needs to be a list so we are proper
; the first conde (nullo) works for w '() 
; the second conde makes w an inproper pair of freshies, the cdro does the same, => (_.0 . _.1)
; the third conde first makes w an inproper pair of freshies,
;   then consosos the head _.0 into a recursive call
;     the first conde works for nullo () => consosing => (_.0)
;     the second conde works as before, another pair => consosing ==> (_.0) + (_.1 . _2) ==> (_.0 _.1 . _.2) 
; etc.

; # 68
(define sorpreso
  (lambda (s)
    (rumbero s '(a b c) '(a b c))))

; returns a goal that will succeed as long as s is not a, b nor c

; # 69
[check-equal?
 (run* (r)
       (== 'd r)
       (sorpreso r))
 '(d)]

; # 70
[check-equal?
 (run* (r) (sorpreso r))
 '(_.0)]

; this is ok, because sorpreso gives a goal, and r is fresh, so there is no contradiction
; because r is not defined/assigned/bound

[check-equal?
 (run* (r)
       (== 'b r)
       (sorpreso r))
 '(b)]

; why is this so? the above is analogous to the following simplified version '(b) instead '(a b c)

[check-equal?
 (run* (r)
       (== 'b r)
       (rumbero r '(b) '(b)))
 '(b)]

; or even simpler

[check-equal?
 (run* (r)
       (rumbero 'b '(b) '(b))) ; shouldn't it fail?
 '(_.0)]

; first, rumbero is a function that returns a goal
; whatever rumbero's returned goal is, it is succeeding for r not becoming '()

; when going through the conde in rumbero, the first one does not apply (nullo)
; the second one fails in the cdro
; the third one works (because the recursion fails and comes back as nullo
; so the above can also be made

[check-equal?
 (run* (r)
       (fresh (result head tail)
              (conso head tail '(b))
              (rumbero 'b tail result)
              (conso head result '(b))
              ))
 '(_.0)]
