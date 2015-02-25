#lang racket
(require "lib/shared.rkt")
(require rackunit)

; This is an addendum to chapter 13 to play with let/cc and understand it

; MY FIRST APPROACH:
;
; The way I visualize it. All lets are frames / boxes / scopes / space-time continuum frames of reference
; Inside the (let/cc we define a scope with two things:
;   - 1st: definition ==> the exit of a worm-hole        :: the same as with letrecc
;   - 2nd: value      ==> the code as it is evaluated    :: the same as with letrecc
; It works the same as letrecc, but now instead of having the definition available through the recursion,
; the definition is an exit point
; Inside the value part, we can enter the worm-hole by passing something to get through the worm-hole (a return value)


; OTHER WAYS TO LOOK AT CONTINUATIONS:

; Say you're in the kitchen in front of the refrigerator, thinking about a sandwich.
; You take a continuation right there and stick it in your pocket. Then you get some turkey and bread out of the refrigerator
; and make yourself a sandwich, which is now sitting on the counter. You invoke the continuation in your pocket,
; and you find yourself standing in front of the refrigerator again, thinking about a sandwich. But fortunately, there's
; a sandwich on the counter, and all the materials used to make it are gone. So you eat it. :-)

; they are like threads

; A continuation is a data structure that represents the computational process at a given point in the process's 
; execution; the created data structure can be accessed by the programming language, instead of being hidden in the runtime environment. 

; It is like a goto with a parameter that is the value of the computation that transferred the control, 
; But it is entirely unlike a goto, as goto moves the programme counter to another place in the programme text,
; while continuations capture a place in the text AND the entire call stack AND programme state into an object.

; A continuation is a pointer to an instruction within a stack frame. Holding this pointer keeps the frame from
; being garbage collected. This pointer can be used to return control to the stack frame. 
; This works just like a function call, except that control jumps to the middle of the function instead of the beginning.


; I LIKE THIS APPROACH:
; 
; http://www.cs.indiana.edu/~dfried/appcont.pdf
; The continuation for some computation is whatever comes after it, expressed as a function of the result for that computation.
; the ideas is that usually we think of instruction to run in a sequential manner, from left to right,
; outside to inside (parens).
; let/cc helps us stop in the middle of the program and look in the opposite direction: right to left, 
; from the inside towards the outside, like unfolding a sock, looking from the other side of the worm-hole.
; the idea is that it is the same code, from a diferent point of view / reference.
; the best way is through examples.


;; example 1

(module+ test [check-equal? 
  (+ 3
     (let/cc hop               ; <= look from here backwards and outwards and define hop!!
             (/ 4 (hop 5))))
  8])

; step1: create hop as
;   (define hop
;     (lambda (x)
;       (+ 3 x)
;     ))               ; exit at the level of the scope where hop is defined
;
; step 2: evaluate body of hop
;   (/4 (hop 5))
;
;   ... go on...
;   (hop 5)
;
; step 3: call hop as defined on step 1
;   => 8 (and exit)


;; example 2

(module+ test [check-equal? 
  (* (+ (let/cc hop2
                (/ (hop2 5) 4))
        8)
     3)
  39])

; step1: create hop as
;   (define hop2
;     (lambda (x)
;       (* 3 (+ 8 x))
;     ))               ; exit at the level of the scope where hop is defined
;
; step 2: evaluate body of hop
;   (/ (hop2 5) 4)
;
;   ... go on...
;   (hop2 5)
;
; step 3: call hop as defined on step 1
;   => 39 (and exit)


;; example 3

(module+ test [check-equal? 
  (+ (let/cc hop3
             (begin
               (printf "hello world")
               5))
     8)
  13])

; step1: create hop as
;   (define hop3
;     (lambda (x)
;       (+ 3 x)
;     ))               ; exit at the level of the scope where hop is defined
;
; step 2: evaluate body of hop
;   (begin ...)
;
;   ... go on...
;   (printf ...)
;
;   ... go on...
;   5 <= return this || the code block (begin...) evaluates to 5
;   since letc/cc only creates a procedure in a scope you keep on going
;
;   ... go on...
;   (+ 5 8)
;
;   return 13 as it exits


;; example 4

(module+ test [check-equal? 
  (+ 30 
     (let/cc k
             (k 20)))
  50])

; this outputs 50, not 20
; k => (+ 30 x)
; just to show that the escape from k is inside the scope defined by let/cc
