#lang racket
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)

; lets start with some definitions
; a table is something that pairs names with values
;   like ((name1 name2) (val1 val2)) => (car (car l)) goes with (car (cdr l))
; now instead of lists we say that a table is a funtion called table
; how do you find the value of a name in that table?
; ...by passing the name as argument to the table funtion (like a hash)

; the key is that a function acts like a table (and viceversa)
; I pass a an argument/name to the function/table and I get a value in return <=== keep in mind throughout

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        [(eq? name2 name1) value]
        [else (table name2)]))))

; extend gets three arguments, a pair name/value and the table to add it to
;   it gives back a function that accepts as input a name and gives back a value
;   -- just like table!! -- if the name of this new function is the same as the one we used to extend
;   then it gives the value we gave to extend, if not it will just call table as usual
; extend, for all purposes, works like table but to which we have added a new pair!
;   it is a table, just not THE table (with more info).

(define is_it_define?
  (lambda (e)
    (cond
      [(atom? e) #f]                          ; because it shows up like a list (define xxx ...)
      [(atom? (car e)) (eq? (car e) 'define)] ; the first thing in the list must be 'define
      [else #f])))

;(define global-table
;  ... the empty table ...)

;(define *define
;  (lambda (e)
;    (set! global-table
;          (extend
;           (name-of e)                            ; name
;           (box (the-meaning (right-side-of e)))  ; value
;           (global-table)))))                     ; table

; remember that functions are like tables, and lets have a global table of all functions
; for each function name we have an associated something to do
; so when we use *define on an element e, what we are doing is extending the global table
; where the name is (name-of e) and the value is a boxy thing.
; so far so good, but what is box?

(define box
  (lambda (it)                              ; [1]
    (lambda (sel)                           ; [2]
      (sel it (lambda (new) (set! it new))) ; [3]
      )))

(define unbox
  (lambda (proc)                            ; [4]
    (proc (lambda (it2 sel2) it2))))        ; [5]

(define set-box
  (lambda (proc newbie)
    (proc (lambda (it3 something) (something newbie))))) ; [6]

; check the file ch20_boxes.rkt to understand the above deadly-yet-innocent-looking defintions.

(define the-meaning
  (lambda (e)
    (meaning e look-up-in-global-table)))

(define look-up-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *quote
  (lambda (e)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))



;(value (define x 3))

;(define value
;  (lambda (e)
;    ...
;    (cond
;      [(define? e) (*define e)]
;      [else (the-meaning e)])
;    ...))
