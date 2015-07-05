#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch21.rkt")

(provide alwayso salo nevero)

;; 1
(define (anyo g)
  (conde
    [g s#]
    [else (anyo g)]))

;; 4
(define nevero (anyo u#))

;; 5
;; (run 1 (q)
;;            nevero
;;            (== #t q))
;; never returns

;; 6
(check-run 1 (q)
           u#
           nevero
           => '())

;; 7
(define alwayso (anyo s#))

(check-run 1 (q)
           alwayso
           (== #t q)
           => '(#t))

;; 8
;; #s succeeds once. alwayso is success kid. always succeeds. forever.

;; 9
;; (run* (q)
;;   alwayso
;;   (== #t q))
;; never finishes...

;; 10
(check-run 5 (q)
           alwayso
           (== #t q)
           => '(#t #t #t #t #t))

;; 11
(check-run 5 (q)
           (== #t q)
           alwayso
           => '(#t #t #t #t #t))

;; 12
(define (salo g)
  (conde
    [s# s#]
    [else g]))

;; not recursive, but it could be used for recursion

;; 13
(check-run 1 (q)
           (salo alwayso)
           (== #t q)
           => '(#t))

;; 14
(check-run 1 (q)
           (salo nevero)
           (== #t q)
           => '(#t))

;; 15
;; (run* (q)
;;   (salo nevero)
;;   (== #t q))
;; never finishes...

;; 16
;; (run 1 (q)
;;   (salo nevero)
;;   u#
;;   (== #t q))
;; never finishes...

;; 17
;; (run 1 (q)
;;   alwayso
;;   u#
;;   (== #t q))
;; never finishes...

;; 18
;; (run 1 (q)
;;   (conde
;;     [(== #f q) alwayso]
;;     [else (anyo (== #t q))])
;;   (== #t q))
;; never finishes...

;; 19
(check-run 1 (q)
           (condi
             [(== #f q) alwayso]
             [else (== #t q)])
           (== #t q)
           => '(#t))

;; 20
;; (run 2 (q)
;;   (condi
;;     [(== #f q) alwayso]
;;     [else (== #t q)])
;;   (== #t q))
;; never finishes again...

;; 21
(check-run 5 (q)
           (condi
             [(== #f q) alwayso]
             [else (anyo (== #t q))])
           (== #t q)
           => '(#t #t #t #t #t))

;; 22
;; conde => finds all solutions for a given "branch" before moving to the next one.
;; condi => finds a solution in a given "branch" before moving to the next one, then jumps back.

;; 24
(check-run 5 (r)
           (condi
             [(teacupo r) s#]
             [(== #f r) s#]
             [else u#])
           => '(tea #f cup))

;; 25
(check-run 5 (q)
           (condi
             [(== #f q) alwayso]
             [(== #t q) alwayso]
             [else u#])
           (== #t q)
           => '(#t #t #t #t #t))

;; 26
;; (run 5 (q)
;;   (conde
;;     [(== #f q) alwayso]
;;     [(== #t q) alwayso]
;;     [else u#])
;;   (== #t q))
;; never finishes...

;; 28
(check-run 5 (q)
           (conde
             [alwayso s#]
             [else nevero])
           (== #t q)
           => '(#t #t #t #t #t))

;; 29
;; (run 5 (q)
;;   (condi
;;     [alwayso s#]
;;     [else nevero])
;;   (== #t q))
;; never finishes...

;; 31
;; (run 1 (q)
;;   (all
;;     (conde
;;       [(== #f q) s#]
;;       [else (== #t q)])
;;     alwayso)
;;   (== #t q))
;; never finishes...

;; 32
(check-run 1 (q)
           (alli
             (conde
               [(== #f q) s#]
               [else (== #t q)])
             alwayso)
           (== #t q)
           => '(#t))

;; 33
(check-run 5 (q)
           (alli
             (conde
               [(== #f q) s#]
               [else (== #t q)])
             alwayso)
           (== #t q)
           => '(#t #t #t #t #t))

;; 34
(check-run 5 (q)
           (alli
             (conde
               [(== #t q) s#]
               [else (== #f q)])
             alwayso)
           (== #t q)
           => '(#t #t #t #t #t))
;; 35
;; 'i' stands for "interleave"

;; 36
(check-run 5 (q)
           (all
             (conde
               [s# s#]
               [else nevero])
             alwayso)
           (== #t q)
           => '(#t #t #t #t #t))

;; 37
;; (run 5 (q)
;;   (alli
;;     (conde
;;       [s# s#]
;;       [else nevero])
;;     alwayso)
;;   (== #t q))
;; never finishes...

;; 38
;; after we get the first conde success, wo move to never and keep running forever

;; 40
;; yup
(check-run 5 (q)
           (all
             (condi
               [s# s#]
               [else nevero])
             alwayso)
           (== #t q)
           => '(#t #t #t #t #t))

