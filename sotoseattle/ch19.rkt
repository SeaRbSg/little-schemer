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

;(test-case "simply testing weird addition"
;  [check-equal? (weird_add) 8])

(module+ test
  (define t0 (weird_add))    ; we need to run it first to set pepe to hop
  [check-equal? t0 8]        ; also we need to store the result in a temp
  (set! t0 (pepe 3))         ; to make the tests work ??
  [check-equal? t0 6]        ; now we can use pepe as (lambda (x) (+ 3 x)) !!!!
  (set! t0 (pepe 4))
  [check-equal? t0 7]
  (set! t0 (pepe 0))
  [check-equal? t0 3])

(module+ test
  (define t1 (weird_add))
  [check-equal? (* 5 5) 25]
  (set! t1 (* (pepe 2) (pepe 200)))  ; <==========  Oh No, Oh No, Oh No...
  [check-equal? t1 5])               ; <==========  ... OH MY GOD!!! << Shrieeeeek >>

; When we use a continuation it forgets everything around it 
; XX COMMANDMENT == Article of Faith [Don't like this kind of articles]
; WHY ??

; Nevertheless, now deepB starts to make sense!

(set! toppings 'anchovies)
(define deepB
  (lambda (m)
    (if (zero? m) 
        (let/cc jump
          (set! toppings jump) 
          'pizza)
        (cons (deepB (sub1 m)) '()))))

(module+ test
  (define t2 (deepB 3))
  [check-equal? t2 '(((pizza)))]
  (set! t2 (toppings 'pepe))
  [check-equal? t2 '(((pepe)))]
  (set! t2 (deepB 4))
  [check-equal? t2 '((((pizza))))]
  (set! t2 (toppings 'cake))
  [check-equal? t2 '((((cake))))]
  [check-equal? (cons (toppings 'cake) (toppings 'cake)) '((((cake))))]) ; <=== Amnesiac Continuation !!

; lets continue on page 161 with a new deep that uses a collector

(define deep&Co
  (lambda (m k)                                ; k is a collector (chapter 8),
    (cond
      [(zero? m) (k 'pizza)]
      [else
       (deep&Co (sub1 m)
                (lambda (x)                    ; this is the new collector
                  (k (cons x '()))))])))       ;
  
(test-case "161 & 162"
  (define id (lambda (x) x))                   ; we use the identity function to get the ball rolling
  [check-equal? (deep&Co 3 id) '(((pizza)))]
  [check-equal? (deep&Co 0 id) 'pizza])

(define deep&CoB
  (lambda (m k)
    (cond
      [(zero? m)
       (let ()
         (set! toppings k)                     ; exactly the same, but now we remember the collector
         (k 'pizza))]
      [else 
       (deep&CoB (sub1 m) (lambda (x) (k (cons x '()))))])))

(test-case "page 163 & 164"
  [check-equal? (deep&CoB 2 (lambda (x) x)) '((pizza))]
  [check-equal? (toppings 'garlic) '((garlic))]              ; toppings is now the procedure 2-layers
  [check-equal? (deep&CoB 4 (lambda (x) x)) '((((pizza))))]
  [check-equal? (toppings 'garlic) '((((garlic))))]          ; toppings is now the procedure 4-layers
  [check-equal? (cons (toppings (quote cake)) 
                      (toppings (quote cake))) '(((((cake)))) (((cake))))])

; the collector, a procedure, is a shadow of the continuation defined before
; a continuation is to a procedure what a positron is to an electron (??)

; change of course, page 165, remember function defined on chapter 11 & 12

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              [(null? lat) #f]
              [else
               (let ((nxt (car lat)))
                 (or (eq? nxt a) (W nxt (cdr lat))))]))))
    (lambda (lat)
      (if (null? lat)
          #f
          (W (car lat) (cdr lat))))))

(test-case "165"
  [check-false (two-in-a-row? '(1 2 3 4 3))]
  [check-true  (two-in-a-row? '(1 2 3 3 4))])




