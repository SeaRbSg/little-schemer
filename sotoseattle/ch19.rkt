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
  [check-equal? (deep 6) '((((((pizza))))))])

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
; deep(13) ; run once and save the 13er function into the function layers
; then we do layers(napolitana) => napolitana with 13 layers
; then we do layers(carrot)     => carrot with 13 layers

(define toppings 'nothingness) ; like the function above that I called layers

(define deepB_initial
  (lambda (m)
    (if (zero? m)
        (let/cc jump
          (set! toppings jump) ; HA! the reverse function when we reach 0 is (cons (deepB (sub1... [1]
          'pizza)
        (cons (deepB (sub1 m)) '()))))

; [1] But beware that what you get is not only the reverse socket/function but the whole stack stored up
;     to that point. That is why it is not a function but a continuation. Buyers Beware!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! toppings 'nothingness)
(define deepB
  (lambda (m)
    (if (zero? m)
        (let/cc jump
          (set! toppings jump)
          'pizza)
        (cons (deepB (sub1 m)) '()))))

(module+ test
  (define t2 (deepB 3))                 ; we learn how to make 3 layer pizza...           [2]
  [check-equal? t2 '(((pizza)))]
  (set! t2 (toppings 'donutburger))     ; ...and now we can bake a 3 layer donutburger    [3]
  [check-equal? t2 '(((donutburger)))]
  (set! t2 (deepB 4))                   ; we learn how to make 4 layer pizza...
  [check-equal? t2 '((((pizza))))]
  (set! t2 (toppings 'cake))            ; ...and now we can bake a 4 layer cake
  [check-equal? t2 '((((cake))))]
  (set! t2 (cons (toppings 'cake) '())) ; if we cons it...
  [check-equal? t2 '((((cake))))]       ; ...shit !! it doesn't cons it    <=== Amnesiac Continuation !!
  [check-equal? (cons (toppings 'cake) (toppings 'cake)) '((((cake))))]) ; <=== Amnesiac Continuation !!

; WHY WHY WHY WHY Doesn't it remember? => Read ch19_zenspider_spider.rkt for a good toy example
; The continuation toppings stores the stack up to that point. That means that when we call toppings
; [3], we are going back to the stack that was initiated in [2]. So when we call toppings, the current
; stack is substituted (so it forgets all that follows as an amnesiac) with the saved one, => the thing
; shows up in the let/cc jump with a value of 'donutburger, it conses the 3 times and continues from where
; the deepB was called [2], finishing right there because there is nothing else to do.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lets continue on page 161 with a new deep that uses a collector

(define deep&Co
  (lambda (m k)                                ; k is a collector (chapter 8),
    (cond
      [(zero? m) (k 'pizza)]
      [else
       (deep&Co (sub1 m)
                (lambda (x)                    ; this is the new collector
                  (k (cons x '()))))])))

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
; There is no let/cc, it is a completely different beast!

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
  (set! t3 (start-it '()))
  [check-equal? t3 '()]
  (set! t3 (walk '((lint) (chips (chips (with))) fish)))
  [check-equal? t3 'lint])

; The last test is important. It works because once leave is defined, walk works!
; (start-it l) has to be run only once!!
; leave is defined as the stack that was initiated with (start-it '()) and then
; the stack of (lambda (x) x),

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
  (set! t4 (start-it '()))
  [check-equal? t4 '()]
  (set! t4 (waddle '((potatong) (caramelos (chips (with))) fish)))
  [check-equal? t4 'potatong])

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
