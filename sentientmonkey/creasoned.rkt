#lang cKanren

(require cKanren/miniKanren)
(require cKanren/neq)

(provide s# u# check-run* check-run)

(define s# succeed)
(define u# fail)

;; ugh hack because cKanren provides 'fail'
(require (prefix-in t: rackunit))

; test macros
(define-syntax check-run*
  (syntax-rules (=>)
    [(_ (vars ...) rules ... => expect)
     (t:check-equal? (run* (vars ...)
                       rules ...)
                     expect)]))

(define-syntax check-run
  (syntax-rules (=>)
    [(_ n (vars ...) rules ... => expect)
     (t:check-equal? (run n (vars ...)
                       rules ...)
                     expect)]))

