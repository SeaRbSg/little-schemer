#lang racket/base

(require rackunit)
(require minikanren)

(provide s# u# else check-run* check-run)

; redefine succeed and fail to avoid conflict with rackunit
(define s# (== #f #f))
(define u# (== #f #t))
; else. a conde that is always true?
(define else (== #t #t))

; test macros via zenspider
(define-syntax check-run*
  (syntax-rules (=>)
    [(_ (vars ...) rules ... => expect)
     (check-equal? (run* (vars ...)
                         rules ...)
                   expect)]))

(define-syntax check-run
  (syntax-rules (=>)
    [(_ n (vars ...) rules ... => expect)
     (check-equal? (run n (vars ...)
                        rules ...)
                   expect)]))
