#lang racket
(require minikanren)

; redefine succeed and fail to avoid conflict with rackunit
(define s# (== #f #f))
(define u# (== #f #t))
; else. a conde that is always true?
(define else (== #t #t))

(provide s# u# else)
