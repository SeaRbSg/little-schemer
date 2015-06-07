#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

(define anyo
  (lambda (g)
    (conde
      [g %s]
      [else (anyo g)])))

(define nevero (anyo %u))

; (run 1 (q)
;   (nevero
;     (== #t g)))


[check-equal?
  (run 1 (q)
    %u
    nevero)
  '()]

(define alwayso (anyo %s)) ;; Why is this 'useful'?

;; Oh, because it succeeds an arbitrary number of times

[check-equal?
  (run 5 (q)
    (== #t q)
    alwayso)
  '(#t #t #t #t #t)] ;; Wait, why do I not have to unquote the truthy values?


(define salo ;; succeeds-at-least-once-o
  (lambda (g)
    (conde
      [%s %s]
      [else g])))

[check-equal?
  (run 1 (q)
    (salo alwayso)
    (== #t q))
  '(#t)]

[check-equal?
  (run 1 (q)
    (salo nevero) ;; these -o suffixes are getting out of hand
    (== #t q))
  '(#t)]

; (run*  (q)
;   (salo nevero)
;   (== #t q))

; (run 1 (q) ;; HELP! The Reasoned explanation for this is useless
;   (salo nevero)
;   %u
;   (== #t q))

; (run 1 (q) ;; Oh, I kinda get it
;   alwayso
;   %u
;   (== #t q))

; (run 1 (q)
;   (conde
;     [(== #f q) alwayso]
;     [else (anyo (== #t q))]
;   (== #t q)))

[check-equal?
  (run 1 (q)
    (condi
      [(== #f q) alwayso]
      [else (== #t q)])
    (== #t q))
  '(#t)]


[check-equal?
  (run 5 (q)
    (condi
      [(== #f q) alwayso]
      [else (anyo (== #t q))])
    (== #t q))
  '(#t #t #t #t #t)]

[check-equal?
  (run 5 (q)
    (condi
      [(teacupo q) %s]
      [(== #f q) %s]
      [else %u]))
  '(tea #f cup)]


[check-equal?
  (run 5 (q)
    (condi
      [(== #f q) alwayso]
      [(== #t q) alwayso]
      [else %u])
    (== #t q))
  '(#t #t #t #t #t)]

[check-equal?
  (run 1 (q)
    (alli
      (conde
        [(== #f q) %s]
        [else (== #t q)])
      alwayso)
    (== #t q))
  '(#t)]

[check-equal?
  (run 5 (q)
    (alli
      (conde
        [(== #f q) %s]
        [else (== #t q)])
      alwayso)
    (== #t q))
  '(#t #t #t #t #t)]


[check-equal?
  (run 5 (q)
    (all ;; but NOT alli
      (conde
        [%s %s]
        [else nevero])
      alwayso)
    (== #t q))
  '(#t #t #t #t #t)]
