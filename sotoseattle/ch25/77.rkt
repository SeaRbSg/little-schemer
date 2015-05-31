#lang racket
(require "../basic_defs.rkt")
(require "../lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../../lib/mk.rkt")

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (appendo d s res)
         (conso a res out))))))

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

; # 77
[check-equal?
  (run 1 (x)
    (flatuleno x '(a b c)))
  '((a b . c))]

; the first two condes will fails as we enter
; the third conde is:
;   fresh (a d aa dd)
;     (conso a d x)
;     (flatuleno a aa)
;     (flatuleno d dd)
;     (appendo aa dd '(a b c)))))))

; the first question to answer is what is (flatuleno _0 _1)

[check-equal?
  (run 9 (x)
    (fresh (a aa)
      (flatuleno a aa)
      (== a x)))
;   a/d                               aa/dd
; ------------------------------    --------------------------
  '(_.0                           ;  (_.0)
    ()                            ;  ()
    (_.0 . _.1)                   ;  (_.0 _.1)
    (_.0)                         ;  (_.0)
    (_.0 _.1 . _.2)               ;  (_.0 _.1 _.2)
    (_.0 _.1)                     ;  (_.0 _.1)
    (_.0 _.1 _.2 . _.3)           ;  (_.0 _.1 _.2 _.3)
    (_.0 _.1 _.2)                 ;  (_.0 _.1 _.2)
    (_.0 _.1 _.2 _.3 . _.4)       ;  (_.0 _.1 _.2 _.3 _.4)
    )] ; never ends

; up until now, both flatulenos succeed for an infinite stream of cases
; hold the thought, the streams exist, it doesn't mean it is a rabbit hole that
; never finishes, not yet because we still have other goals to evaluate
;
; besides the flatuleno goals, we also have an appendo goal that has to
; succeed: (appendo aa dd '(a b c))
; inside the appendo there are two conditions:
;   - the first one says: if the first element is nullo, the second is the output
;     so for aa==() [we have it as 2nd sol] ==> dd==(a b c)
;     and for the equivalence table above, dd==(a b c) ==> d==(a b . c)
;     and aa==() ==> a==()
;     therefore consosing into x ==> x==(a b . c) FIRST SOL

;     So it is this last goal the one that drives the execution (order of
;     solutions). The first conde here that succeeds, means it is the first
;     time that ALL goals succeed, therefore the first solution.
;
;   - the second condition:
;      ((fresh (a d res)
;         (conso a d aa)
;         (appendo d dd res)
;         (conso a res '(a b c)))))))
;     Consider that up to this point we have determined aa, dd, and (a b c)
;     This will succeed when it exits successfully, when it reaches the nullo
;     conde in a recursion call => when d is nullo, and res==dd
;     And from the equivalence table, d==() ==> dd==()
;     so a==(a b c) and (conso a d x) ==> x==(a b c) SECOND SOL

;     Again, this is the second consecutive case in which ALL goals succeed
;     and therefore shows up as second solution
;     The key is that we juggle, we have all stream solutions on the air
;     and only when we evaluate ALL conditions simultaneously in the block
;     (inside the (fresh (a d aa dd)...)) we follow the order of successes

; Consider that not only the 2 flatulenos give us infinite streams of possible
; solutions, but the appendo too!
;
; As we recur inside the appendo, I become utterly lost and desperate
; I imagine that it is because the nullo in the recursive appendo is never hit
; again, and it never exits the rabbit hole

; Now, consider that there are OTHER solutions to the flatulencio problem
; there are values from the aa and dd streams that satisfy the problem
; for example:

[check-equal?
  (run* (x)
    (flatuleno '((a) b c) '(a b c))
    (== #t x))
  '(#t)]

; or...

[check-equal?
  (run* (x)
    (flatuleno '((a b) c) '(a b c))
    (== #t x))
  '(#t)]

; the problem is that they are not used because the appendo went rabbit-holing
; and they never came up for use!!
