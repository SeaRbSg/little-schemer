#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

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
      ((fresh (a d aa dd y w z)
         (conso a d s)
         (flatuleno a aa)
         (flatuleno d dd)
         (appendo aa dd out))))))

; # 77
[check-equal?
  (run 2 (x)
    (flatuleno x '(a b c)))
  '((a b . c)(a b c))]

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

; It goes DEPTH FIRST and sequentially, although it takes into consideration the existance of
; these kind-of-lazy stream to remember how we iterate over them
; It is sequential (flatuleno a aa) -> (flatuleno d dd) -> (appendo)
;
; stream_aa.each do |sol_aa|
;   strem_dd.each do |sol_dd|
;     appendo sol_aa sol_dd '(a b c)
;   end
; end

; So I get the first solution of (flatteno a aa) => aa == `(,a)
;   Then take the first of (flatteno d dd) wich is also dd == `(,d)
;   I try to appendo and fail
;   Then I try with the SECOND sol of (flatteno d dd) => dd == d == ()
;   Try to appendo and fail
;   The I try the THIRD (flatteno d dd) => dd==(_0. _.1) and d==(_.0 . _.1)
;   Try to appendo and succeed!! => First solution (a b . c) where a==(a) & d==(b . c)
;   Try for next sol of (flatteno d dd) => dd==(_.0) ==> fail appendo (as before)
;   Try for next sol of (flatteno d dd) => dd==(_.0 _.1 _.2) ==> fail appendo too
;   Try for next sol of (flatteno d dd) => dd==(_.0 _.1) and d==(_.0 _.1) propper
;   The appendo works again! ==> Second solution
;   ...The rest of solutions of (flatenno d dd) will fail in the appendo
;      and since there are infinte solutions for dd, it will never stop

; The proof that this explanation is valid can be walked through with the following
; modified flatulenos, where we substitute each flatuleno recursive call with the
; solution that we want to verify

(define flatuleno_sol_1
  (lambda (s out)
    (conde
      ((conso s '() out))
      ((nullo s) (== s out))
      ((fresh (a d aa dd y w z)
         (conso a d s)
         ;(flatuleno a aa)
         (== `(,a) aa)
         ;(flatuleno d dd)
         (== d  `(,w . ,z))        ; FIRST SOL
         (== dd `(,w ,z))          ; FIRST SOL
         (appendo aa dd out))))))

[check-equal?
  (run 1 (x)
    (flatuleno_sol_1 x '(a b c)))
  '((a b . c))]

(define flatuleno_sol_2
  (lambda (s out)
    (conde
      ((conso s '() out))
      ((nullo s) (== s out))
      ((fresh (a d aa dd y w z)
         (conso a d s)
         ;(flatuleno a aa)
         (== `(,a) aa)
         ;(flatuleno d dd)
         (== d  `(,w ,z))           ; SECOND SOL
         (== dd `(,w ,z))           ; SECOND SOL
         (appendo aa dd out))))))

[check-equal?
  (run 1 (x)
    (flatuleno_sol_2 x '(a b c)))
  '((a b c))]
