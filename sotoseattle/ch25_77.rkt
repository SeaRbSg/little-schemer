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

; and the (appendo _.0 _.1 '(a b c)) also can be extracted and analyzed
[check-equal?
  (run 4 (q)
    (fresh (x y)
      (appendo x y '(a b c))
      (== `(,x ,y) q)))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ()))] ; <--- solutions taken from here ???
;   ==> a 5th solution is never found ==> infinite-loop-entrance
;       this infinite loop is not an infinite stream of sols like the above flatulenos
;       it is just a black hole, once inside no solution is found, ever...more

; Not all solutions of aa and dd are compatible with the appendo or first conso
; Yet, there are more than 2. The following values work:
;
; |   aa     |  dd     |  a        |   d       | s: (conso a d)
; +----------+---------+-----------+-----------+----------------
; |   (a)    | (b c)   | (a)       | (b c)     | ((a) b c)
; |   (a)    | (b c)   | (a)       | (b . c)   | ((a) b . c)
; |  (a b)   |  (c)    | (a b)     | c         | ((a b) c)
; |          |  (c)    | (a . b)   | c         | ((a . b) c)
; | (a b c)  |  ()     | (a b . c) | ()        | (a b . c)    <== 1st sol found
; | (a b c)  |  ()     | (a b c)   | ()        | (a b c)      <== 2nd & last sol
; |   ()     | (a b c) | ()        | (a b . c) | (() a b . c)
; |   ()     | (a b c) | ()        | (a b c)   | (() a b c)

[check-equal?
  (run* (x)
      (flatuleno '((a) b c)     '(a b c))
      (flatuleno '((a) b . c)   '(a b c))
      (flatuleno '((a b) c)     '(a b c))
      (flatuleno '((a . b) c)   '(a b c))
      (flatuleno '(a b . c)     '(a b c))
      (flatuleno '(a b c)       '(a b c))
      (flatuleno '(() a b . c)  '(a b c))
      (flatuleno '(() a b c)    '(a b c))
      (== #t x))
  '(#t)]

; Trying to figure it out we could express the code substituting the function
; calls for the streams they represent...
;   ...
;   fresh (a d aa dd)
;     (conso a d x)
;     ((_.0) () (_.0 _.1) (_.0) (_.0 _.1 _.2) (_.0 _.1)...) ; <== aa
;     ((_.0) () (_.0 _.1) (_.0) (_.0 _.1 _.2) (_.0 _.1)...) ; <== dd
;     ((()(a b c)) ((a) (b c)) ((a b) (c)) ((a b c)) ∞)     ; <== (aa dd)

; One way to look at it is from the stream of aa, I pick the first one, then I pick
; the first of dd, and finally I try to append ==> fail => try the next dd,......
; It would be sequential (flatuleno a aa) -> (flatuleno d dd) -> (appendo)
; That would be total DFS and it cannot be the solution because it would never end going through dd
;
; Another way would be to consider the streams in parallel, a la BFS
; I would combine an aa with all dd, and try appendo for each pair
; But then i) other solutions would show up before (a b . c) and ii) I would not get lost in infinity

; So, no idea yet...

