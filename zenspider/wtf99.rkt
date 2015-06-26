#lang racket/base

(require "lib/reasonable.rkt")

(define (cons° a d l)
  (dbg "cons°"
       (≈ (cons a d) l)))

(define (pair° p)
  (fresh (a d)
    (dbg "pair°"
         (cons° a d p))))

(define (null° l)
  (dbg "null°"
       (≈ '() l)))

(define (append° l s out)               ; 9
  (dbg "append°"
       (cond-e [(null° l) (≈ s out)]
               [(fresh (a d res)
                  (cons° a d l)          ; deconstruct l
                  (cons° a res out)      ; build (cons a res) => out
                  (append° d s res))]))) ; always recurse last

(define (flatten° s out)              ; 59
  (dbg "flatten°"
       (cond-e [(null° s) (≈ s out)]
               [(pair° s) (fresh (a d res1 res2)
                            (cons° a d s)
                            (flatten° a res1)
                            (flatten° d res2)
                            (append° res1 res2 out))]
               [(cons° s '() out)])))

(run* (x) (flatten° '(b) x))

