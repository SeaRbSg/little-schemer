#lang racket
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require minikanren)

(define s# (== #f #f))
(define u# (== #f #t))

; Starting from the start, this much we know

[check-equal?
 (run* (q)
       (== 'luis q))
 '(luis)]

; q is a fresh variable, that is successfuly bound to 'luis
; (== 'luis q) is a goal
; a goal is a procedure, lambda, function
; run* evaluates goals, every goal, executing the goals (lambdas) and checking if they succeed or not

[check-equal?
 (run* (q)
       (== 'pepe q)
       (== 'luis q))
 '()]

; q as a fresh variable, cannot be bound twice. 
; run* evaluates all goals. 
; The first goal (== 'pepe q) succeeds
; but the second (== 'luis q) fails (because a fresh var can only be bound once)
; this ilogical/unatenable/impossible situation (one fresh var bound to 2 things) means
; that q is neither pepe nor luis, is null, '(), or WK-WTF

; Now for the conundrum. Why the following code works? Why doesn't it blow?
; why the q is not '()? 

[check-equal?
 (run* (q)
       (begin
         (== 'pepe q)
         (== 'luis q)))
 '(luis)]

; it is the same as before, only that now we wrap the goals in a begin expression
; the same happens with:

[check-equal?
 (run* (q)
       (let ()
         (== 'pepe q)
         (== 'luis q)
         ))
 '(luis)]

; or even worse...

[check-equal?
 (run* (q)
       (let ()
         (== #t #f)     ; this should fail
         u#             ; blow dammit
         (== 'pepe q)
         (== 'luis q)
         ))
 '(luis)]

; it looks like the let is only evaluating the last expression... yes and no...
; the above is similar to:
; (run* (q)
;       (let () g1 g2 g3 g4))
;
; g1 g2 g3 g4 are goals, lambdas, functions, the let goes through them but it does not evaluate them
; it is just 'enumerating' a bunch of functions (goals), without executing them,
; and it returns the last one g4
;
; remember that run* evaluates goals,
; as it gets to the let, the let goes over a bunch of functions (functions as data or arguments)
; the let returns back g4 (== 'luis q) and run* runs it:
; binds q with luis because q is fresh and has never been bound before, => succeeds
; so q is 'luis

; Moral of the story: Never forget that Goals are Lambdas.
; Don't see goals as run-of-the-mill expressions evaluated on the run (as you read them), 
; but as functions to be evaluated (only when called by run*).