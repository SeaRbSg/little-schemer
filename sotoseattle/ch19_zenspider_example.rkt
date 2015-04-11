; Thanks ZenSpider!
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

(printf "-------CHAPTER I----------~n")


(define (go1)
  (set! val-1 (deepB 2))
  (printf "--------- go1 ----------~n")
  (printf "  go1 -- deepB 2 == ~a~n" (deepB 2))
  (printf "  ................. ~a~n" (deepB 2))
  (printf "  go1 -- val-1   == ~a~n" val-1)
  (printf "  go1 -- val-2   == ~a~n" val-2)
  )

(define (go2)
  (printf "hola from go2!~n")
  (set! val-2 (toppings 'mozzarella)) 
  (printf "--------- go2 ----------~n")                              ; obliterated
  (printf "  go2 -- toppings 'mozz == ~a~n" (toppings 'mozzarella))  ; obliterated
  (printf "  go2 -- val-2   == ~a~n" val-2)                          ; obliterated
  )

(printf "running go1..............~n")
(go1)                                    ; the stack starts here (see below)
(printf "now running go2..........~n")
(go2)
(printf "........ended running go2~n~n~n")

; Important to note that when we define the continuation, we are storing the stack up to that point
; This means that toppings (jump) holds not only the stack of the defined deepB, but more importantly
;   the stack of whoever called it to set up m (in this case it was go1 who called deepB with m = 2)
; That is why go2 does prints out the stuff of go1. Because as it continues with the stored stack
;   it goes to whoever called deepB last (the dotted printf!! not the previous printf or the set!) 
;   and goes on from there printing!!

; As the call to the continuation substitutes one current stack from the previously stored one,
;   of go2 we only see the printed 'hola', all the rest is forgotten (stack is substituted)

(printf "-------CHAPTER II----------~n")

;; the above is equivalent to:

(define (go/finite)
  (call-with-continuation-prompt go1)
  (call-with-continuation-prompt go2))

(go/finite)

(printf "-------CHAPTER III----------~n")

(define (go/infinite)
  (go1)                 ; the stack NOW starts in the commented out (go/infinte) and comes here!!!
  (go2))

;(go/infinite)

; In this case there is no barrier between the go1 and go2.
; The stack stored in the continuation starts as before from the top most level (go/infinite),
;   then goes to the go1 call inside, but now that call is continued with a call to go2,
;   so everytime it gets into go2 => (as before) it goes into go1 and as it exits go1, 
;   enters that next-door go2 again doing an infinite loop (enter go2 --> exit go1 --> enter go2...)

(printf "~ndone~n")

;; http://docs.racket-lang.org/reference/module.html?q=call%2Fcc#%28form._%28%28quote._~23~25kernel%29._module%29%29

;; Then, expressions and definitions are evaluated in order as they
;; appear within the module. Each evaluation of an expression or
;; definition is wrapped with a continuation prompt (see
;; call-with-continuation-prompt) for the default prompt tag and using
;; a prompt handler that re-aborts and propagates its argument to the
;; next enclosing prompt.