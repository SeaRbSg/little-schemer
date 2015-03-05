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
    (set! x food)
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
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

; define binds the name omnivore to the result of the let, which is a lambda (a procedure)
; with the added internal definion of x as 'minestrone
; now, x is an internal name !!! in the let (not the x defined above)

(module+ test
  (check-equal? x 'potato)) ; <=== from our last test dinerR !! <> minestrone

; So to avoid conflicts SIXTEENTH COMMDANDMENT: use set! only with names defined in lets

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

; QUESTION: Why does the book make a fuzz about the difference between that internal x referring
; to something but not (never) having a value? Isn't it the same? Doesn't x have a value while
; gobbler or omnivore executes? And refer also to nothing (aka doesnt exist) out of the scope?






