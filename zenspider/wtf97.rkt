#lang racket/base

(require "lib/reasonable.rkt")

(run* (x)
      (cond-e [(≈ 'tea x)]
              [(≈ 'cup x)]))

