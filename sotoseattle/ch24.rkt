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

; # 18
[check-equal?
 (run 4 (z)
      (fresh (u)
             (memo 'tofu `(a b tofu d tofu e . ,z) u)))
 '(_.0 _.0 (tofu . _.0) (tofu . _.0))
]

; when it reaches the first tofu
; [conde 1] 'tofu == 'tofu && '(tofu d tofu e . _.0) == u ==> OK SOL #1 _.0
; [conde 2] get down ... to the second tofu
;   [conde 1] car ok && '(tofu e . _.0) == u ==> OK SOL #2 _.0
;   [conde 2] (memo 'tofu '(e . ,z) u) ... drill down till z
;   [conde 2] (memo 'tofu '(,z) u)
;     [conde 1] caro is 'tofu << means l has both caro and cdro, '(,z) == '(tofu . _.1)
;               && '(tofu . _.1) == u ==> OK SOL #3 '(tofu . _.0)
;     [conde 2] (memo 'tofu '(_.1) u), which is the same as before
;       [conde 1] 



