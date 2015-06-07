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
  #| (lambda-limited 3 (s out) |#
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
  (run 9 (a)
    (fresh (aa)
      (flatuleno a aa)))
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

; there more than 2 solutions to the flatulencio problem
; there are values from the aa and dd streams that satisfy the problem

[check-equal?
  (run* (x)
    (conde
      ((flatuleno '((a) b c)   '(a b c)) )  ; a==(a)       d==(b c) ??
      ((flatuleno '((a b) c)   '(a b c)) )  ; a==(a b)     d==c     ??
      ((flatuleno '((a . b) c) '(a b c)) )  ; a==(a . b)   d==c     ??
      ((flatuleno '(a b . c)   '(a b c)) )  ; a==(a b . c) d==()    SOL 1
      ((flatuleno '(a b c)     '(a b c))   ); a==(a b c)   d==()    SOL 2
      )
      (== #t x))
  '(#t #t #t #t #t)]

[check-equal? ; proof that '((a) b c) solves and a/aa d/dd hit in the process
  (run* (q)
    (conso '(a) '(b c) '((a) b c))
    (appendo '(a) '(b c) '(a b c))
    (flatuleno '(a) '(a))
    (flatuleno '(b c) '(b c))
    (== q #t))
  '(#t)]

[check-equal? ; proof that '((a b) c) solves and a/aa d/dd hit in the process
  (run* (q)
    (conso '(a b) 'c '((a b) . c))
    (appendo '(a b) '(c) '(a b c))
    (flatuleno '(a b) '(a b))
    (flatuleno 'c '(c))
    (== q #t))
  '(#t)]

; could it be the appendo who is directing the search/walk?
(run 4 (q)
  (fresh (x y)
    (appendo x y '(a b c))
    (== `(,x ,y) q)))
; '((() (a b c))
;   ((a) (b c))
;   ((a b) (c))
;   ((a b c) ()) <--- solutions taken from here ???
;   ==> infinite-loop-entrance)

; No idea yet...

; MY PAST EXPLANATION ==> WRONG!!
; up until now, both flatulenos succeed for an infinite stream of cases
;
; So the (flatuleno a aa) gets first possible solution a==_.0 and aa==(_.0)
; then it goes for (flatuleno d dd) and for the first solution it tries to append
; and fails. Then it tries the next sol of (flatuleno a aa) => a==()==aa
; and walks the stream of solutions of (flatuleno d dd) untile d==(_.0 _.1 _.2)
; it again tries to append, and BINGO! it works ==> x==(a b . c) FIRST SOL

; We have found a solution in the first element of (flatuleno a aa) and the
; second solution of (flatuleno d dd). NOW, instead of keeping exploring other
; solutions to d (keeping a fixed) => which would get us into infinite loop,
; we go back up to (flatuleno a aa) and keep exploring solutions of a/aa
; ====> NOT DEPTH FIRST SEARCH <====

; We keep trying and failing until we hit a==(a b c), at which time we look into
; (flatuleno d dd) and keep going until d==()==dd, for which the appendo works
; and the consing of x works too ==> x==(a b c) the SECOND SOL
;
; We go back up to (flatuleno a aa) and keep working the stream of possible
; solutions continuing with a==(_.0 _.1 _.2 _.3 . _.4) and we will never stop
; because from no onwards no solutions exist that satisfy all goals

