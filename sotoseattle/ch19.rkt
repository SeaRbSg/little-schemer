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

; ... and now let's try to come up with a way to reverse this sock.

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

; not what we want, let's rehash it in a different way:
; if we used deep(49), and somehow, at the end we saved a function that conses 49 times...
; ... then we could use that saved function on any other argument (napolitana, carrot, etc) and get
; 49 layered different kind of pizzas!!
;
; deep(49) ; run once and save the 49er function into the function layers
; then we do layers(napolitana) => napolitana with 49 layers
; then we do layers(carrot)     => carrot with 49 layers
; deep(13) ; run once and save the 49er function into the function layers
; then we do layers(napolitana) => napolitana with 13 layers
; then we do layers(carrot)     => carrot with 13 layers

(define toppings 'nothingness) ; like the function above that I called layers

(define deepB_initial
  (lambda (m)
    (if (zero? m)
        (let/cc jump
          (set! toppings jump) ; HA! the reverse function when we reach 0!!
          'pizza)
        (cons (deepB (sub1 m)) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
           ;(print 'hola)    ; it would print because it continues evaluating
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nevertheless, now deepB starts to make sense!

(set! toppings 'nothingness)
(define deepB
  (lambda (m)
    (if (zero? m)
        (let/cc jump
          (set! toppings jump)
          'pizza)
        (cons (deepB (sub1 m)) '()))))

(module+ test
  (define t2 (deepB 3))                 ; we learn how to make 3 layer pizza...
  [check-equal? t2 '(((pizza)))]
  (set! t2 (toppings 'donutburger))     ; ...and now we can bake a 3 layer donutburger
  [check-equal? t2 '(((donutburger)))]
  (set! t2 (deepB 4))                   ; we learn how to make 4 layer pizza...
  [check-equal? t2 '((((pizza))))]
  (set! t2 (toppings 'cake))            ; ...and now we can bake a 4 layer cake
  [check-equal? t2 '((((cake))))]
  (set! t2 (cons (toppings 'cake) '())) ; if we cons it...
  [check-equal? t2 '((((cake))))]       ; ...shit !! it doesn't cons it    <=== Amnesiac Continuation !!
  [check-equal? (cons (toppings 'cake) (toppings 'cake)) '((((cake))))]) ; <=== Amnesiac Continuation !!

; WHY WHY WHY WHY Doesn't it remember?
; the continuation toppings includes all the conses and would be analogous to:
; (set! toppings
;   (lambda (specialty)
;     (cons_m_times specialty)))
; The explanation is obvious: if let/cc creates a wormhole, that implies a micro black hole
; at the exit of the black hole, (where we call (toppings whatever) all that other stuff is
; inside the Schwarzschild radius, by definition all that stuff falls in the black hole
; and the only stuff to get out is whatever we sent through the wormhole. That is why it ignores
; whatever surrounds the call to (toppings and only considers the first one (obvious, wasn't it?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; the collector is a procedure, not a continuation.
; Now toppings is a procedure, a pale shadow of the continuation defined before.
; There is no let/cc (no Schwarzschild radius!!), a completely different beast!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define leave '())
(define walk
  (lambda (l)
    ;(print 'indeed!)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (leave (car l))] ; [4][5]
      [else
       (let ()
         (walk (car l))
         (walk (cdr l)))])))

(define start-it
  (lambda (l)
    (let/cc here                        ; [1]
      (set! leave here)                 ; [2]
      (walk l))))                       ; [3]

; (start-it l)
;   - creates a wormhole exit (here)     [1]
;   - references it outside unto leave   [2]
;   - walks the l, which in turn:        [3]
;       - finds the first atom
;       - calls (leave found_atom) == (here found_atom):                   [4]
;             - sends the found_atom through that entrance of the wormhole [5]
;   - the found_atom shows up in the wormhole exit [1], and it is over!    [1]

; the idea is that there are two ways to define the exit of the wormhole:
; inside a function: page 167 - def leftmost -> skip but then it is only available inside leftmost
; outside the function: page 167 - def start-it -> here setting it to leave defined outside, 
; the continuation is now available outside walk

(module+ test
  (define t3 (start-it '((potatoes) (chips (chips (with))) fish)))
  [check-equal? t3 'potatoes]
  ;;;
  (set! t3 (start-it '()))
  [check-equal? t3 '()]
  ;;;
  (set! t3 (walk '((lint) (chips (chips (with))) fish)))
  [check-equal? t3 'lint])

; The last test is important. It works because once leave is defined, walk works!
; (start-it l) has to be run only once!!
; leave is defined as something like: (define leave (lambda (x) (walk l))) where l is open
; so (walk l2) will use l2, not the l defined in the initial (start-it '((potato) (...)))
; that is the power of continuation, no commitment, a free willing libertarian of a procedure

;;; page 169

(define fill 'hola)

; This is easier to understand at first blush (it is the same, just combined)

(define start-it-catch22
  (lambda (l)
    (let/cc here                             ; [a]
      (letrec
          ((W (lambda (l)
                (cond
                  [(null? l) '()]
                  [(atom? (car l))
                   (let ()
                     (let/cc catch_it        ; [b] for fill
                       (set! fill catch_it)
                       (here (car l)))       ; exit at [a] with the first car
                     (W (cdr l)))]           ; catch_it/fill is cont defined as (lambda (x) (W (cdr l)))
                  [else
                   (let ()
                     (W (car l))
                     (W (cdr l)))]))))
        (W l)))))

(module+ test
  (define t22 (start-it-catch22 '((donuts) (cheerios (cheerios (spaguettios))) donuts)))
  [check-equal? t22 'donuts]
  (set! t22 fill))

; now that we see the light, let's continue the disembowelment...

(define waddle
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (let ()
         (let/cc catch_it
           (set! fill catch_it)
           (leave (car l)))
         (waddle (cdr l)))]
      [else
       (let ()
         (waddle (car l))
         (waddle (cdr l)))])))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(module+ test
  (define t4 (start-it2 '((donuts) (cheerios (cheerios (spaguettios))) donuts)))
  [check-equal? t4 'donuts]
  (set! t4 fill)
  ;;;
  (set! t4 (start-it '()))
  [check-equal? t4 '()]
  ;;;
  (set! t4 (waddle '((potatong) (caramelos (chips (with))) fish)))
  [check-equal? t4 'potatong])

; (sarcasm 'obvious!)

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill 'gone))))          ; we said before that catch_it/fill is defined as (lambda (x) (W (cdr l)))

(module+ test
  (define t5 (get-next 'go))
  [check-equal? t5 'caramelos] ; that is == W of ((caramelos (chips (with))) fish)
  (set! t5 (fill 'irrelevant)) ; again, we get the car of the cdr
  [check-equal? t5 'chips]
  (set! t5 (fill 'f_off))      ; and again, the car of the cdr
  [check-equal? t5 'with]
  (set! t5 (fill 'f_off))      ; and again, the car of the cdr
  [check-equal? t5 'fish]
  (set! t5 (fill 'f_off))      ; and again, but now we are stuck! uh-ho! that aint good
  [check-equal? t5 'fish])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let's fix the bug. For comparison purposes we have:
;
; (define start-it
;   (lambda (l)
;     (let/cc here
;       (set! leave here)
;       (walk l))))

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave '()))))

(module+ test
  (define t6 (get-first '(1 (2))))
  [check-equal? t6 '1]
  (set! t6 (get-next 'go))
  [check-equal? t6 '2]
  (set! t6 (get-next 'go))
  [check-equal? t6 '()]            ; now it works when it reaches the end
  (set! t6 (get-next 'go))
  [check-equal? t6 '()])           ; and keeps on working

; now let's go back and redefine two-in-a-row*. For comparison purposes here what we had for lats
;
; (define two-in-a-row?
;  (letrec
;      ((W (lambda (a lat)
;            (cond
;              [(null? lat) #f]
;              [else
;               (let ((nxt (car lat)))
;                 (or (eq? nxt a) (W nxt (cdr lat))))]))))
;    (lambda (lat)
;      (if (null? lat)
;          #f
;          (W (car lat) (cdr lat))))))

(define two-in-a-row*?_v0
  (letrec
      ((W (lambda (a)
            (let ((nxt (get-next 'go)))
              (if (atom? nxt)
                  (or (eq? nxt a) (W nxt))
                  #f)))))
    (lambda (l)
      (let ((fst (get-first l)))
        (if (atom? fst)
            (W fst)
            #f)))))

(module+ test
  [check-true  (two-in-a-row*?_v0 '(1 1))]
  [check-true  (two-in-a-row*?_v0 '(1 (1)))]
  [check-true  (two-in-a-row*?_v0 '(1 () () (1)))]
  [check-true  (two-in-a-row*?_v0 '(1 ((1))))]
  [check-false (two-in-a-row*?_v0 '(1 (2 (1))))])

; final refactoring with everything pretty-packaged

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
            (let ((nxt (get-next 0)))
              (if (atom? nxt)
                  (or (eq? nxt a) (T? nxt))
                  #f))))
       (get-next
        (lambda (x)
          (let/cc here-again
            (set! leave here-again)
            (fill 'go))))
       (fill (lambda (x) x))
       (waddle
        (lambda (l)
          (cond
            [(null? l) '()]
            [(atom? (car l))
             (let ()
               (let/cc rest
                 (set! fill rest)
                 (leave (car l)))
               (waddle (cdr l)))]
            [else
             (let ()
               (waddle (car l))
               (waddle (cdr l)))])))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst)
            (T? fst)
            #f)))))

(module+ test
  [check-true  (two-in-a-row*? '(1 1))]
  [check-true  (two-in-a-row*? '(1 (1)))]
  [check-true  (two-in-a-row*? '(1 () () (1)))]
  [check-true  (two-in-a-row*? '(1 ((1))))]
  [check-false (two-in-a-row*? '(1 (2 (1))))])
