#lang racket/base

(define toppings #f)
(define (deepB m)
  (cond [(zero? m)
         (let/cc jump
                 (set! toppings jump)
                 'pizza)]
        [else (cons (deepB (sub1 m)) '())]))

(define val-1 #f)
(define val-2 #f)

(define (wtf? n)
  (printf "go~a: val-1 = ~a~n" n val-1)
  (printf "go~a: val-2 = ~a~n" n val-2))
(define (go1)
  (set! val-1 (deepB 2))                ; '((mozzarella))
  (wtf? 1))
(define (go2)
  (set! val-2 (toppings 'mozzarella))
  (wtf? 2))

(go1)
(go2)

;; equivalent to:

(define (go/finite)
  (call-with-continuation-prompt go1)
  (call-with-continuation-prompt go2))

(go/finite)

(define (go/infinite)
  (go1)
  (go2))

;; (go/infinite) -- because there is no continuation barrier in-between 1&2

(printf "done~n")

;;; prints:

;; go1: val-1 = ((pizza))
;; go1: val-2 = #f
;; go1: val-1 = ((mozzarella))
;; go1: val-2 = #f
;; go1: val-1 = ((pizza))
;; go1: val-2 = #f
;; go1: val-1 = ((mozzarella))
;; go1: val-2 = #f
;; done

;; http://docs.racket-lang.org/reference/module.html?q=call%2Fcc#%28form._%28%28quote._~23~25kernel%29._module%29%29

;; Then, expressions and definitions are evaluated in order as they
;; appear within the module. Each evaluation of an expression or
;; definition is wrapped with a continuation prompt (see
;; call-with-continuation-prompt) for the default prompt tag and using
;; a prompt handler that re-aborts and propagates its argument to the
;; next enclosing prompt.
