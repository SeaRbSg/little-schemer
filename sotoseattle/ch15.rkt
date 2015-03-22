#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require test-engine/racket-tests)
;(require racket/trace)

; ONE, we can define more than procedures

(define y
  (cons 'chicago (cons 'pizza '())))

(module+ test
  (check-equal? y '(chicago pizza)))

; TWO, set doesnt exist in a vacuum it needs the name to be defined beforehand
; (set! x3 'whatever) will fails saying that x3 is unbound

(define x
  (cons 'chicago (cons 'pizza '())))

(set! x 'gone)

(set! x 'skins)

(module+ test
  (check-equal? x 'skins))

; THREE, (define and (set! dont evaluate to anything, dont return anything at all !! <=======

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))

(module+ test
  (check-equal? (gourmet 'onion) '(onion skins)))

; FOUR, so far it is pretty easy...

(define gourmand
  (lambda (food)
    (set! x food)              ; setting x to food inside the lambda (internal scope)
    (cons food (cons x '()))))

(module+ test
  (check-equal? (gourmand 'potato) '(potato potato)))

; FIVE, given diner write a dinerR that remembers last meal

(define diner
  (lambda (food)
    (cons 'milkshake (cons food '()))))

(define dinerR
  (lambda (food)
    (set! x food)                        ; x now stores!! the food (setter)
    (cons 'milkshake (cons food '()))))

(module+ test
  (check-equal? (diner 'potato)  '(milkshake potato))
  (check-equal? (dinerR 'potato) '(milkshake potato)))

; SIX, as we store state we need a way to avoid conflicts

(define omnivore
  (let ((x 'minestrone))                ; the definition
       (lambda (food)                   ; the value (code to run taking into consideration the definition)
         (set! x food)
         (cons food (cons x '())))))

; define binds the name omnivore to the result (value) of the let, which is a lambda (a procedure)
; with the added internal definion of x as 'minestrone
; now, x is an internal name !!! in the let (not the x defined above)

(module+ test
  (check-equal? x 'potato)) ; <=== from our last test dinerR !! <> minestrone

; So to avoid conflicts SIXTEENTH COMMDANDMENT: use set! only with names previously defined in lets

(define gobbler1
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)              ; setting x to food inside the let (internal scope)
      (cons food (cons x '())))))

(define gobbler2
    (lambda (food)
      (set! x food)              ; setting x globally (no letting scope)
      (cons food (cons x '()))))

(module+ test
  [check-equal? x 'potato]                            ; from our latest test
  (check-equal? (gobbler1 'lemons) '(lemons lemons))
    [check-equal? x 'potato]                          ; doesn't change
  (check-equal? (gobbler2 'lemons) '(lemons lemons))
    [check-equal? x 'lemons])                         ; changes (no let)

;(print x)

; QUESTION 1: Why does the book make a fuzz about the difference between that internal x referring
; to something but not (never) having a value? Isn't it the same? Doesn't x have a value while
; gobbler or omnivore executes? And refer also to nothing (aka doesnt exist) out of the scope?
; as Billie would say: potatoe <> potato ?
; Actually no!, There are... 2 answers 
;   => there are no variables (that have value), only pointers that refer to stuff
;   => x inside the scope refers to something, but outside has no value

(define nibbler1
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food (cons x '())))))

(define nibbler11
  (lambda (food)
    ((lambda (x)
      (set! x food)
      (cons food (cons x '()))) 'donut)))

(define nibbler2
  (lambda (food)
      (set! x food)
      (cons food (cons x '()))))

(define nibbler3
  (let ((x 'whatever))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define nibbler33
  ((lambda (x) 
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))) 'whatever))

(define nibbler4
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food (cons x '())))))

(module+ test
  [check-equal? x 'lemons]                              ; from before
  [check-equal? (nibbler1 'trout) '(trout trout)]
  [check-equal? x 'lemons]                              ; does not remember/change (lambda > let > set)

  [check-equal? (nibbler11 'trout) '(trout trout)]
  [check-equal? x 'lemons]                              ;lambda == let
  
  (check-equal? (nibbler2 'cheerio) '(cheerio cheerio))
  [check-equal? x 'cheerio]                             ; remembers/changes (lambda > set)

  (check-equal? (nibbler3 'lutefisk) '(lutefisk lutefisk))
  [check-equal? x 'cheerio]                             ; does not remember/change (let > lambda > set)

  (check-equal? (nibbler4 'lutefisk) '(lutefisk lutefisk))
  [check-equal? x 'cheerio])                            ; does not remember/change (lambda > let > set)

; let > lambda > set === lambda > let > set because:
;   1.- in both ways the outter-scoped x is not changed (only the private x changes for a bit)
;   2.- let is a lambda !!

(define food '())

(define glutton
  (lambda(x)
    (set! food x)
    (cons 'more (cons x (cons 'more (cons x '()))))))

(module+ test
  [check-equal? (glutton 'pepe) '(more pepe more pepe)]
  [check-equal? food 'pepe]
  [check-equal? x 'cheerio])

(define chez-nouz
  (lambda ()
    (set! food x)
    (set! x food)))

(module+ test
  [check-equal? (glutton 'trippe) '(more trippe more trippe)]
  [check-equal? food 'trippe]                                  ; glutton changed the food
  (chez-nouz)
  [check-equal? food 'cheerio]                                 ; food << previously defined x
  [check-equal? x 'cheerio])                                   ; x << food << previously defined x

(chez-nouz)

; 18 COMM: Use set! only when you no longer care about what x held

(define chez-nuts
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(module+ test
  [check-equal? (glutton 'eel) '(more eel more eel)]
  [check-equal? food 'eel]                                  ; glutton changed the food
  (chez-nuts)
  [check-equal? food 'cheerio]                              ; food << previously defined x
  [check-equal? x 'eel])                                    ; x << a << pre-previously defined food!

; I prefer the ruby way: x, y = y, x
