#lang racket/base

(define box
  (lambda (it)                              ; [1]
    (lambda (sel)                           ; [2]
      (sel it (lambda (new) (set! it new))) ; [3]
      )))

(define yosegi (box 3))

; Things to understand once and for all:
;   - box is a function that takes one argument ('it')
;   - the first lambda [1] creates a closure where 'it' has a value. This means that whatever is defined inside
;     always has acces to the value of 'it'
;   - box returns a function without a name, a lambda [2]. We will call it IAF (internal anonymous function)
;     IAF takes a single argument, another function called 'sel' (so far we have 3 functions)
;     IAF returns whatever is the result of calling sel on two arguments...
;     ... with the added insult that the second argument to sel, in the process sets it to whatever 
;         the second argument was (which is in itself a fourth lambda/closure/function)
;     IAF knows what is the value of 'it' because of what we sadi before

(define unbox
  (lambda (proc)                            ; [4]
    (proc (lambda (it2 sel2) it2))))        ; [5]

; unbox is a function with a single argument, another function (proc) [4]
; unbox returns whatever results from calling that proc on a single arguments, the result of the passing 
;   two arguments it2, sel2 and only returning 'it2' in lambda [5]
;   so it is the same as (proc it) # the convoluted way is to be able to pass 2 args but use only one?

(unbox yosegi)

; so when I say (unbox yosegi) I am passing to unbox the argument yosegui, which is a function [2]
; and [5] calls (yosegi sel) where sel is (lambda (it2 sel2) it2) => return the first of the two arguments
; and [3] does  (sel it (lambda (new) (set! it new))) is calling sel on 2 args 'it' and 'new' inside another lambda
;               forget about the second argument, we know that this sel returns the first one
;               and since the first arg it2 [5] is actually 'it' [3], and yosegui knows 'it'
;               voila! the whole thing gives back 'it' from [1]
; fuck! what a trip!

(define set-box
  (lambda (proc newbie)
    (proc (lambda (it3 something) (something newbie))))) ; [6]

(set-box yosegi 8)

; so when I say (set-box yosegi 8) I am passing to set-box the arguments: yosegui (which is a function [2]) and 8
; and [6] calls (yosegi sel) where sel is (lambda (it3 something) (something newbie)) => return  (second_arg newbie)
; and [3] does  (sel it (lambda (new) (set! it new))) is calling sel on 2 args 'it' and ['new' inside another lambda]
;               we know that this sel returns (something newbie)
;               as newbie is evaluated the enclosing lambda acts! (lambda (newbie) (set! it newbie))
;               and it is set! (forget about something like with unbox it doesn't matter)
; so with unbox we focus on the first arg of sel and with set-box we focus on the second

(unbox yosegi)
