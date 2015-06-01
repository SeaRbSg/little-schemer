#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

; # 1 - 3
(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

; if g succeeds, it succeeds infinitely (rabbit holing)
[check-equal?
  (run 1 (q) (anyo s#))
  '(_.0)]

; # 4 - 6
(define nevero (anyo u#))

; (run 1 (q)
;   nevero
;   (== #t q)) ; never ends because the exit conde never hits and
;   recurs infinitively

[check-equal?
  (run* (q)
    u#
    nevero)
  '()]

; # 7 - 11
(define alwayso (anyo s#))

[check-equal?
  (run 1 (q)
    alwayso
    (== #t q))
  '(#t)]

; alwayso is a `stream` of s#, an infinite list of succeesses (instead of one)
; (run* (q) alwayso (== #t q)) ; never ends

[check-equal?
  (run 5 (q)
    alwayso
    (== #t q))
  '(#t #t #t #t #t)]

[check-equal?
  (run 5 (q)
    (== #t q)
    alwayso)
  '(#t #t #t #t #t)]

; # 12 - 15
(define salido      ; lo bueno si breve, dos veces bueno
  (lambda (g)
    (conde
      (s#)
      (g))))

[check-equal?
  (run 1 (q)
    (salido alwayso)
    (== #t q))
  '(#t)]

[check-equal?
  (run 1 (q)
    (salido nevero)  ; (define nevero (anyo u#))
    (== #t q))
  '(#t)]
; the first conde of salido works, then it goes rabbit-holing

; # 16
;  (run 1 (q)
;  (salido nevero)  ; (ok nope nope nope nope nope...)
;  u#               ; (nope)
;  (== #t q))
;
; I think that because salido is a conde what we have is an infinite
; succession of: (ok-nope nope-nope nope-nope nope-nope nope-nope nope-nope...)
; and the == is never reached

; # 17
;  (run 1 (q)
;  alwayso          ; (ok ok ok ok ok ok...)
;  u#               ; (nope)
;  (== #t q))
; idem eadem idem

; # 18
;  (run 1 (q)
;    (conde
;      ((== #f q) alwayso) ; q == (#f #f #f #f #f #f...) fails with q==#t ==> rabbit hole
;      ((anyo (== #t q)))) ; q == (#t #t #t #t #t #t...) but never reached!!
;  (== #t q))
; idem eadem idem

; # 19 - 20
[check-equal?
  (run 1 (q)
    (condi
      ((== #f q) alwayso) ; q == (#f #f #f #f #f #f...) fails with q==#t ==> rabbit hole
      ((== #t q)))        ; q == #t
     (== #t q))
  '(#t)]
; condi instead of going down rabbit hole, when a condition doesnt work, it
; alternates and tries the next condition!
; after the first fuckup in alwayso, it changes to the other one and tries

; after condinterleaving the first time, on the second, instead of staying
; on that condition, it changes lanes again, and in this case it goes bonkers
; once more time (run 2 ==> hang up

; # 21 - 23
[check-equal?
  (run 3 (q)
    (condi
      ((== #f q) alwayso) ; q == (#f #f #f #f #f #f...) fails with q==#t
      ((anyo (== #t q)))) ; q == (#t #t #t #t #t #t...) agrees!!
     (== #t q))
  '(#t #t #t)]
; when having two `streams`, it changes lanes every time (or only when fails)?

; # 24
(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) s#]
     [(== 'cup x) s#]
     [u#])))

; remember how this one works (from ch21)
[check-equal?
  (run* (r) (teacupo r))
  '(tea cup)]

[check-equal?
  (run 5 (r)
    (condi
      ((teacupo r)) ; r == (tea cup)
      ((== #f r))   ; r == #f
      (u#)))        ; fail
  '(tea #f cup)]
; as we interleave, we have in order: tea, #f, SHIT => go next interleave: cup

; # 25
[check-equal?
  (run 5 (q)
    (condi
      ((== #f q) alwayso) ; q = (#f #f #f ...) will always fail
      ((== #t q) alwayso) ; q = (#t #t #t ...) ok
      (u#))               ; will always Fail
    (== #t q))
  '(#t #t #t #t #t)]
; fail->ok, Fail->fail->ok, Fail->fail->ok, Fail->fail->ok, Fail->fail->ok

; # 26 - 27
; (run 5 (q)
;   (conde
;     ((== #f q) alwayso) ; q = (#f #f #f ...) down the rabbit hole...
;     ((== #t q) alwayso) ; never reaches
;     (u#))               ; never reaches
;   (== #t q))

; # 28 - 29
[check-equal?
  (run 5 (q)
    (conde
      (alwayso s#) ; (ok ok ok ok ....)
      (nevero))    ; never reached (......... never succeeding, never stopping)
    (== #t q))
  '(#t #t #t #t #t)]

; # 30
[check-equal?
  (run 1 (q)
    (condi
      (alwayso s#) ; (ok ok ok ok ....)
      (nevero))    ; never reached (......... never succeeding, never stopping)
    (== #t q))
  '(#t)]
; a run 1 would give us the first #t
; but because it is a condi, the next condition evaluated is the nevero
; which never ends. A run n > 1 will go bonkers

; # 31
; (run 1 (q)
;   (all             ; here this is repetitive, because in run, all goals must pass
;     (conde
;       ((== #f q))  ; q == #f
;       ((== #t q))) ; never reached
;     alwayso)       ; (ok ok ok ...)
;   (== #t q))       ; q == #t BOOM!, go back to next alwayso ok
; an infinite succession of the same failure

; # 32 - 33
[check-equal?
  (run 4 (q)
    (alli            ; all interleaved
      (conde
        ((== #f q))  ; q == #f
        ((== #t q))) ; q == #t
      alwayso)       ; (ok ok ok ...)
    (== #t q))       ; q == #t BOOM!, go back to next conde!!
  '(#t #t #t #t)]

; # 34 - 35 swapping condes
[check-equal?
  (run 4 (q)
    (alli
      (conde
        ((== #t q))  ; q == #t
        ((== #f q))) ; q == #f will not work
      alwayso)       ; (ok ok ok ...)
    (== #t q))       ; q == #t works! go on to next alwayso (which never ends)
  '(#t #t #t #t)]

; # 36
[check-equal?
  (run 5 (q)
    (all
      (conde
        (s#)      ; ok
        (nevero)) ; (....) but never reached
      alwayso)    ; (ok ok ok ...)
    (== #t q))
  '(#t #t #t #t #t)]
; ok x (ok ok ok ...) ==> (#t #t #t #t ...)

; # 37 - 39
; (run 5 (q)
;   (alli
;     (conde
;       (s#)      ; ok
;       (nevero)) ; (....)
;     alwayso)    ; (ok ok ok ...)
;   (== #t q))
; run 1 works with (#t)
; but the second time around we enter the second conde (interleave),
; and we never get out of the hole again
