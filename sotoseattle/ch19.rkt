#lang racket
(require "lib/shared.rkt")
(require rackunit)

;;; page 155 & 156

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m)) '()))))

(test-case "page 155 & 156"
  [check-equal? (deep 6) '((((((pizza))))))]
)

;;; page 157

; we have defined deep passing as argument how many layers we want, and the name is constant
; we can define the same but making the name variable, but the depth constant

; the key is that shit happens ONLY when we reach m == 0. Only then we start consing as possessed
; so let's play with the long version for 3 layers of pizza...

(define 3-layers
  (lambda (name_of_pizza)
    (cons
      (cons
        (cons name_of_pizza '())
      '())
    '())))

(test-case "page 157 a"
  [check-equal? (3-layers 'napolitana) '(((napolitana)))])

; ... and now let's try to come up with a way to reverse this sock

(define missguided_effort
  (lambda (n_layers pizza_type)
    (letrec
        ((D (lambda (m)
                  (if (zero? m)
                      pizza_type
                      (cons (D (sub1 m)) '())))))
        (cond
          [(zero? n_layers) pizza_type]
          [else (cons (D (sub1 n_layers)) '())]))))

(test-case "page 157 b"
  [check-equal? (missguided_effort 3 'napolitana) '(((napolitana)))])

; not what we want, let try strating from deep and use set!

(define toppings 'anchovies)

(define deepB_initial
  (lambda (m)
    (if (zero? m) 
        (let/cc jump
          (set! toppings jump) 
          'pizza)
        (cons (deepB (sub1 m)) '()))))
  
; this use of let/cc seems freaky at first. Revisit ch13_bis.rkt and the last approach to understand it.
; http://www.cs.indiana.edu/~dfried/appcont.pdf
; ---------------------------------------------
; The continuation for some computation is whatever comes after it, expressed as a function of the result 
; for that computation. The ideas is that usually we think of instruction to run in a sequential manner, 
; from left to right, outside to inside (parens).
; let/cc helps us stop in the middle of the program and look in the opposite direction: right to left, 
; from the inside towards the outside, like unfolding a sock, looking from the other side of the worm-hole.
; the idea is that it is the same code, from a diferent point of view / reference the best way is through example.

(test-case "let/cc as seen from the other side"
  [check-equal? 
   (+ 3
     (let/cc hop               ; <= look from here backwards and outwards and define hop!!
             (/ 4 (hop 5))))
   8])

; hop is a function!! But it is internal, it cannot be accessed from outside the scope, so the only way to
; capture it is by defining a name outside and then referencing with set!

(define pepe '())
(define weird_add
  (lambda ()
    (+ 3 (let/cc hop
           (set! pepe hop)
           (/ 4 (hop 5))
           ))))

(test-case "simply testing weird addition"
  [check-equal? (weird_add) 8])

(module+ test
  (weird_add)               ; we need to run it first to set pepe to hop
  [check-equal? (pepe 3) 6] ; now we can use pepe as (lambda (x) (+ 3 x)) !!!!
  [check-equal? (pepe 4) 7]
  [check-equal? (pepe 0) 3])

; now deepB makes sense!

(set! toppings 'anchovies)
(define deepB
  (lambda (m)
    (if (zero? m) 
        (let/cc jump
          (set! toppings jump) 
          'pizza)
        (cons (deepB (sub1 m)) '()))))

(test-case "page 158, 159, 160"
  [check-equal? toppings 'anchovies]
  [check-equal? (deepB 3) '(((pizza)))])

(module+ test
  (deepB 3)
  [check-equal? (toppings 'pepe) '(((pepe)))]
  (deepB 6)
  [check-equal? (toppings 'cake) '((((((pepe))))))])

; The XX Commandment sounds funny the way the books says it. I rather use the analogy of the reversed sock!
