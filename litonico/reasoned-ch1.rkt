;; The purpose of minikanren is to find values that satisfy some conditions.
;;
;; (run* (var1 var2 ...) (condition1 condition2 ...))
;; means "Run this minikanren program"
;; and it will return a list of values that satisfy all of the conditions.
;; Or, if there's no such value, for example:
;;
;; (run* (q)    ; we're looking for some q that satisfies...
;;  (== q #t)   ; "q must be true"
;;  (== q #f))  ; "q must be false, too!"
;;
;; run* will return '(), meaning, "no value satisfies this".


;; WTF is fresh?
;;
;; (fresh (x) ...)
;; means "Let x be arbitrary in the following expression"

#lang racket

(require minikanren)
(require rackunit)
(require "reasoned-prelude.rkt")

(run* (q) %u)

(run* (q)
  (== #t q))

(run* (q)
  %u
  (== #t q))

(run* (q)
  %s
  (== #t q))

(run* (r)
  %s
  (== 'corn r))

(run* (r)
  %u
  (== 'corn r))

(run* (r)
  %s
  (== #f r))

(run* (r)
  (let ((x #t))
    (== #f x)))

(run* (r)
  (let ((x #f))
    (== #f x)))

(run* (q)
  (fresh (x)
         (== #t x)
         (== #t q)))

(run* (q) ; yay associativity
  (fresh (x)
         (== x #t)
         (== #t q)))

(run* (q) %s)

(run* (x)                                                               ; p. 8
  (let [(x #f)]
    (fresh (x)
      (== #t x))))

(run* (r)
  (fresh (x y)
    (== (cons x (cons y '())) r)))

(run* (r)
  (fresh (t u)
    (== (cons t (cons u '())) r)))

(run* (r)
  (fresh (x)
    (let [(y x)]
      (fresh (x)
        (== (cons y (cons x (cons y '()))) r)))))

(run* (q)                                                               ; p. 9
  (== #f q)
  (== #t q))

(run* (q)
  (== #f q)
  (== #f q))

(run* (q)
  (let [(x q)]
    (== #t x)))

(run* (r)
  (fresh (x)
    (== x r)))

(run* (q)
  (fresh (x)
    (== #t x)
    (== x q)))

(run* (q)
  (fresh (x)
    (== x q)
    (== #t x)))

(run* (q)                                                               ; p. 10
  (fresh (x)
    (== #t x)
    (== x q)))

(cond
  [#f #t]
  [else #f])

(run* (q)
  (cond
    [#f %s]
    [else %u]))

(run* (q)                                                               ; p. 11
  (conde
    [%u %s]
    [%u]))

(run* (q)
  (conde
    [%u %u]
    [%s]))

(run* (q)
  (conde
    [%s %s]
    [%u]))

(run* (x)
  (conde
    [(== 'olive x) %s]
    [(== 'oil x) %s]
    [%u]))

(run 1 (x)                                                              ; p. 12
  (conde
    [(== 'olive x) %s]
    [(== 'oil x) %s]
    [%u]))

;; I don't understand this one-- shouldn't x be "anything but 'virgin"?
;; Also, why ('olive '(_.0) 'oil)? Since there's a '(_.0) in there, shouldn't
;; x be "absolutely anything"?
(run* (x)
  (conde
    [(== 'virgin x) %u]
    [(== 'olive x) %s]
    [%s %s]
    [(== 'oil x) %s]
    [%u]))

(run 2 (x)                                                              ; p. 13
  (conde
    [(== 'extra x) %s]
    [(== 'virgin x) %u]
    [(== 'olive x) %s]
    [(== 'oil x) %s]
    [%u]))

(run* (r)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (== (cons x (cons y '())) r)))

(run* (r)
  (fresh (x y)
    (conde
      [(== 'split x) (== 'pea y)]
      [(== 'navy x) (== 'bean y)]
      [%u])
    (== (cons x (cons y '())) r)))

(define teacupo                                                         ; p. 14
  (lambda (x)
    (conde
      [(== 'tea x) %s]
      [(== 'cup x) %s]
      [%u])))

(run* (x)
  (teacupo x))

[check-equal?
  (run* (r) ; Missing a (#f #t)... why????
            ; Also, why does Aja's code work, but not mine?
    (fresh (x y)
      (conde
        [(teacupo x) (== #t y) %s]
        [(== #f y) (== #t y)]
        [%u])
      (== (cons x (cons y '())) r)))
  '((#f #t) (tea #t) (cup #t))]

[check-equal?
  (run* (r)
    (fresh (x y z)
      (conde
        [(== y x) (fresh (x) (== z x))]
        [(fresh (x) (== y x)) (== z x)]
        [%u])
      (== (cons y (cons z '())) r)))
  '((_.0 _.1) (_.0 _.1))]


[check-equal?                                                           ; p. 15
  (run* (r)
    (fresh (x y z)
      (conde
        [(== y x) (fresh (x) (== z x))]
        [(fresh (x) (== y x)) (== z x)]
        [%u])
      (== #f x)
      (== (cons y (cons z '())) r)))
  '((#f _.0) (_.0 #f))]

(run* (q)
  (let [(a (== #t q))
        (b (== #f q))]
    b))

(run* (q)
  (let [(a (== #t q))
        (b (fresh (x)
             (== x q)
             (== #f x)))
        (c (conde
             [(== #t q) %s]
             [(== #f q)]))]
    b))
