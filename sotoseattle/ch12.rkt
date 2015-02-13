#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require racket/trace)

; Try to write multirember in your own

(define my-multirember
  (lambda (a lat)
    (cond                                                      ; A
      [(null? lat) lat]                                        ; B
      [(eq? a (car lat)) (my-multirember a (cdr lat))]         ; C
      [else (cons (car lat) (my-multirember a (cdr lat)))])    ; D
    ))


(module+ test
  [check-equal? (my-multirember 'a '(v a c a w a)) '(v c w)]
  [check-equal? (my-multirember 'sardines '(sardines)) '()])

; So now we want to avoid referencing the atom a every time we recurse
; In the best possible world we would want to be able to do this:
;
; (define multirember
;   (lambda (a lat)
;     (mr lat)))
;
; (define mr 
;   (lambda (lat)
;     (cond
;       [(null? lat) lat]
;       [(eq? a (car lat)) (mr (cdr lat))]
;       [else (cons (car lat) (mr (cdr lat)))])))
;
; Because it would allow us to plug and play procedures considering variables in scope
; But it doesn't work because a in mr is undefined :: 'unbound identifier'

; The key concept is that the name of an argument only matters inside the (lambda...)
; Because names themselves can change, if I (define multirember (lambda b lat...)), then 
; (define mr (lambda lat... a...)) won't know what a is referring to!
; the question is how do make sure that the a of mr is the same as the a of multirember
; how can we define the scope of stuff ==> enter letrec

(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond                                          ; == A
                  [(null? lat) lat]                            ; == B
                  [(eq? a (car lat)) (mr (cdr lat))]           ; == C but recurse on mr
                  [else (cons (car lat) (mr (cdr lat)))])      ; == D but recurse on mr
           )))
       mr)
     lat)))

; ((letrec code-of-procedure name-of-procedure))
; ((letrec ((mr...     ))    mr))
; ((letrec ((sausage...))    sausage))
;
; Page 19, 4th down & right col it says:
;  " It separates the two parts...: the naming part which is the nexted box, from the value part, which is mr"
; Should it not be the opposite? the code is the value, and the mr is just the name?
; NOOOOOOO! The value is the result of the letrec, in this case mr
;
; go back to...
;
; (define multirember
;   (lambda (a lat)
;     (mr lat)))
;
; now substitute that mr for the following letrec

; (letrec
;          ((mr (lambda (lat)
;                 (cond                                          ; == A
;                   [(null? lat) lat]                            ; == B
;                   [(eq? a (car lat)) (mr (cdr lat))]           ; == C but recurse on mr
;                   [else (cons (car lat) (mr (cdr lat)))])      ; == D but recurse on mr
;            )))
;        mr)
;
; and we get the right code
; where we had mr, now we have a whole letrec expression that returns... mr
; fuck, this is twisted!
;
; the naming part of a letrec uses whatever variables are defined in the scope of the surrounding lambda (a)
; the value of the letrec, in this case, is a recursive function that 'sees' a.

; written in another way:

(define multirember-v2
  (lambda (a lat)
    (letrec
         ((mr (lambda (lat)
                (cond
                  [(null? lat) lat]
                  [(eq? a (car lat)) (mr (cdr lat))]
                  [else (cons (car lat) (mr (cdr lat)))]))))
         (mr lat)
    )))

; Is the same, now the value of the letrec is (mr lat) => applying the defined mr to lat, instead of 
; just mr (which we later apply to lat)
; In this way, the value returned by the letrec is all that multirember is and does

; REVIEW ==> How does letrec compare to Y combinator?

; THE TWELFTH COMMANDMENT: Use letrec to remove arguments that don't change in recursion








