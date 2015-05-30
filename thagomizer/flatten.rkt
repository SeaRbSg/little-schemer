#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

;; (define flatteno
;;   (lambda (s out)
;;     (conde
;;      [(nullo s) (== '() out)]                 ;; 59a
;;      [(pairo s)                               ;; 59b
;;       (fresh (a d res-a res-d)
;;              (conso a d s)
;;              (flatteno a res-a)
;;              (flatteno d res-d)
;;              (appendo3 res-a res-d out))]
;;      [else (conso s '() out)])))              ;; 59c

(define appendo3
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d rest)
             (conso a d l)
             (conso a rest out)
             (appendo3 d s rest))))))

(define flatteno
  (lambda (s out)
    (conda 
     [(nullo s) (== '() out)]
     [else
      (fresh (a d res-a res-d)
             (conso a d s)
             (conda
              [(pairo a)
               (flatteno a res-a)
               (flatteno d res-d)
               (appendo3 res-a res-d out)]
              [else
               (flatteno d res-d)
               (conso a res-d out)]))])))
