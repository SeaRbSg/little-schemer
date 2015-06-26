#lang racket/base

(require "lib/reasonable.rkt")

(define (teacup° x)
  (dbg "teacup°"
       (cond-e [(≈ 'tea x) %s]
               [(≈ 'cup x) %s])))

(run* (x) (teacup° x))

