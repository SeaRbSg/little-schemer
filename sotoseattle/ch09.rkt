#lang racket
(require "lib/shared.rkt")
(require "ch6.rkt")
(require "ch7.rkt")
(require rackunit)
(require racket/trace)

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define pick ; from chapter 4
  (lambda (pos lat)
    (cond
      [(zero? (sub1 pos)) (car lat)]
      [else (pick (sub1 pos) (cdr lat))])))

(define keep-looking
  (lambda (a sym_num lat)
    (cond
      [(number? sym_num) (keep-looking a (pick sym_num lat) lat)]
      [else (eq? sym_num a)])))

;(trace keep-looking)
(module+ test
  [check-true  (looking 'caviar '(6 2 4 caviar 5 7 3))]
  [check-false (looking 'caviar '(6 2 grits caviar 5 7 3))])

; the above recursion does not follow the letter of the Law (of recursion)
; it does not recurse (?) on a part of lat, it uses the whole lat!
; this means => unnatural recursion
; the problem of looking is that it may end in an endless loop
; because of this => partial function (vs. what we have seen which are total functions)
; partial means that for some inputs, there is no output (i.e. because never outputs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eternity
  (lambda (sleep_of_the_just)
    (eternity sleep_of_the_just)))

; (eternity 10) never ends, keeps going for eternity

; it is an unholy and unnatural recursion because it defies the law
; it is a partial function because it never finishes
; also it does not blow up the stack!!
;   it is a single box stack because it has call tail optimization
;   the return of the function is whatever the last call returns and that is a call to itself.
;   the call to eternity keeps substituting the call to eternity in the stack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a-pair?, build, first and second defined and imported from ch7
(define shift
  (lambda (pair)
    (build (first (first pair)) 
           (build (second (first pair)) (second pair)))))

(module+ test
  (check-equal? (shift '((a b) c)) '(a (b c)))
  (check-equal? (shift '((a b) (c d))) '(a (b (c d)))))

; input is a pair (where car is also pair),
; output is a new pair where:
;  - 1st element = car of the first element of input (eoi)
;  - 2nd element = pair of (2nd of first eoi, whole 2nd eoi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define align
  (lambda (x)
    (cond
      [(atom? x) x]
      [(a-pair? (first x)) (align (shift x))]
      [else (build (first x) (align (second x)))])))

(module+ test
  (check-equal? (align 'q)             'q)
  (check-equal? (align '(q b))         '(q b))
  (check-equal? (align '(q (b c)))     '(q (b c)))
  (check-equal? (align '((q w) b))     '(q (w b)))
  (check-equal? (align '((q w) (b c))) '(q (w (b c)))))


(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora)) (length* (second pora)))])))

(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (x 2 (weight* (first pora))) (weight* (second pora)))])))

(module+ test
  (check-equal? (weight* (align 'q))             1) ; q
  (check-equal? (weight* (align '(q b)))         3) ; (q b)
  (check-equal? (weight* (align '(q (b c))))     5) ; (q (b c))
  (check-equal? (weight* (align '((q w) b)))     5) ; (q (w b))
  (check-equal? (weight* (align '((q w) (b c)))) 7)); (q (w (b c)))

(define shuffle
  (lambda (x)
    (cond
      [(atom? x) x]
      [(a-pair? (first x)) (shuffle (revpair x))]
      [else (build (first x) (shuffle (second x)))])))


(module+ test
  (check-equal? (shuffle 'q)             'q)
  (check-equal? (shuffle '(q b))         '(q b))
  (check-equal? (shuffle '(q (b c)))     '(q (b c)))
  (check-equal? (shuffle '((q w) b))     '(b (q w))))
;  (check-equal? (shuffle '((q w) (b c))) '(q (w (b c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define will-stop?
  (lambda (f)
    (cond
      [(f '()) #t]
      [else #f])))

(module+ test
  [check-true (will-stop? length)])
; nono 
; (will-stop? (eternity '()))

(define last-try
  (lambda (x)
    (and (will-stop? last-try) (eternity x))))

; don't even try this at home
; (will-stop? (last-try '()))

; will-stop? is a function that we can define in our minds, that we can express with words,
; but that we can NOT define with Scheme <== define

; the rest of the chapter is the derivation of the Y combinator
; no need to code it here, but to follow with paper and pen
; another good input for this: http://mvanier.livejournal.com/2897.html

; Here are my notes about it:
;
; 1. Why Y?
;    When we write a recursive function, the gist of it is that the function calls itself
;    so the function has a name, a name that the function itself calls from inside
;    The problem is, how do we do that if the function has no name, if it is anonymous?
;    Y => allows us to do just that.
;
; 2. What is Y?
;    It is just a function. It takes a function as input and outputs another function.
;    The input function is non-recursive function and the output function is recursive!!
;    Aside: what is a combinator? A lambda with no free variables
;           a lambda that can be substituted for its name and everything is defined
;
; 3. How does Y work?
;    Given the following function
     (define count
       (lambda (lat)
         (cond
           [(null? lat) 0]
           [else (add1 (count (cdr lat)))])))

;    this is the same but without the recursive call
;    because of the 11th Commandment
;    WHEN IN DOUBT => ABSTRACT IT OUT AND MAKE IT A PARAMETER TO THE FUNCTION
     (define almost-count
       (lambda (f)
         (lambda (lat)
           (cond
             [(null? lat) 0]
             [else (add1 (f (cdr lat)))]))))

;    So imagine that we already have the Y function ready to go, then
;    (Y almost-count) would output a fully functional recursive count, as
     (define count (Y almost-count))
;
; 4. Derivation of Y
;    lets try to write recursively with almost-count
;    we are going to need a function that we pass as argument (that freaky f):
     (define whatever
       (crash-computer-with-fireworks))

;    the idea here is that we don't care about f, we just want it to be something
;    that will never be called (bear with me)
;    then would have that the original definition works for an empty list
     (define count_empty_list (almost-count whatever))

;    if we want it to work for both empty list and lists with a single element we
;    would insert again as recursion
     (define count_up_to_1_element
       (almost-count
         (almost-count whatever)))

;    so what we want is something like this
     (define count_any_list
       (almost-count
         (almost-count
           (almost-count
             ...

;    Detour: Fixpoint
;    for a function f, input and output are the same
;    for f = x^2, 1 is a fixpoint because f(1) = 1, input == output for that value
;    for a given f, (f fixpoint) = fixpoint
;    the fixpoint can be a number or a function
     (almost-count fixpoint) = fixpoint

;    reversing the order
     fixpoint = (almost-count fixpoint)

;    substituting fixpoint for its expanded version...
     fixpoint = (almost-count (almost-count fixpoint))

;    again...
     fixpoint = (almost-count
                  (almost-count
                    (almost-count fixpoint))
;    OMG!!!
     fixpoint = (almost-count
                  (almost-count
                    (almost-count
                      (almost-count
                        ...

;    So THE FIXPOINT FUNCTION OF ALMOST-COUNT IS COUNT
;
;    How do we get that fixpoint function?
;    ...well, we know from before that as we have defined it
     (Y almost-count) = count

;    , so...
     (Y f) = fixpoint-of-f

;    and we know that
     fixpoint = (almost-count fixpoint)

;    so substituting one for the other
     (Y f) = fixpoint-of-f = (f fixpoint-of-f)
     (Y f) = (f fixpoint-of-f)
     (Y f) = (f (Y f))

;    That's it
     (define (Y f) (f (Y f)))

;    or nicely formatted
     (define Y
       (lambda (f)
         (f (Y f))))

;    but that is for lazy languages. For Scheme or Ruby we need to make it lazy
;    which means wrapping the function call inside another lambda that doesn't
;    actually do anything (just delays evaluation until it's being called)
     (Y f) = (f (lambda (x) ((Y f) x))) ; or

     (define Y
       (lambda (f)
         (f (lambda (x) ((Y f) x)))))

     (define Y (lambda (f) (f (lambda (x) ((Y f) x)))))

;    wrapping up
     (define count (Y almost-count))

;    substituting (Y f) for its definition
     (define count (almost-count (lambda (x) ((Y almost-count) x))))

;    or going back to our initial count definition
     (define count
       (lambda (lat)
         (cond
           [(null? lat) 0]
           [else (add1 ((lambda (x) ((Y almost-count) x)) (cdr lat)))]))

;    Up to this point we have Y, but not the Y Combinator, because Y refers to itself!

; 5. Derivation of the Y Combinator
;    lets go back to our initial definition
     (define count
       (lambda (lat)
         (cond
           [(null? lat) 0]
           [else (add1 (count (cdr lat)))])))

;    we start by applying the 11 commandment
     (define partial-count
       (lambda (self lat)
         (cond
           [(null? lat) 0]
           [else (add1 (self (cdr lat)))])))

;    but that is not correct, we need to call it with the extra parameter
     (define partial-count
       (lambda (self lat)
         (cond
           [(null? lat) 0]
           [else (add1 (self self (cdr lat)))])))

;    let's make it more clear by separating the arguments
     (define partial-count
       (lambda (self)
         (lambda (lat)
           (cond
             [(null? lat) 0]
             [else (add1 ((self self) (cdr lat)))]))))


     ((count count) '(1 2 3))
     (define count (partial-count partial-count))
     (count '(1 2 3))
;    This is a valid definition of count based on partial-count WITHOUT recursion!
;    BEWARE: this is all for lazy languages, to simplify, for strict ones, just wrap in lambdas
;
;    Changing tracks to follow (Y Y) Works! from Mathias Felleisen & Daniel Friedman
;
;    so we can do this to wrap everything together in a single expression
;    (f g) => evaluate f with g as passing argument
     ((lambda (partial-count)                                 ; <== f
        (partial-count partial-count))
      (lambda (partial-count)                                 ; <== g
        (lambda (lat)
          (cond
            [(null? lat) 0]
            [else (add1 ((partial-count partial-count) (cdr lat)))]))))

;    Lets wrap the (self self) thing in a neutered lambda
     ((lambda (partial-count)                                 ; <== f
        (partial-count partial-count))
      (lambda (partial-count)                                 ; <== g
        (lambda (lat)
          (cond
            [(null? lat) 0]
            [else (add1 ((lambda (x) ((partial-count partial-count) x)) (cdr lat)))]))))

;    now we move it out
     ((lambda (partial-count)                                 ; <== f
        (partial-count partial-count))
      (lambda (partial-count)                                 ; <== g
        (
          (lambda (count)
            (lambda (lat)
              (cond
                [(null? lat) 0]
                [else (add1 (count (cdr lat)))]))
          (lambda (x) ((partial-count partial-count) x)))
        )
      )
     )


; we can still extract the counter function farther out
   ((lambda (co)
     ((lambda (partial-count)                                 ; <== f
        (partial-count partial-count))
      (lambda (partial-count)
        (co (lambda (x) ((partial-count partial-count) x))))))
     (lambda (count)                                          ; <==g
       (lambda (lat)
         (cond
           [(null? lat) 0]
           [else (add1 (count (cdr lat)))])))
   )

; let's separate f
   (lambda (co)
     ((lambda (partial-count)
        (partial-count partial-count))
      (lambda (partial-count)
        (co (lambda (x) ((partial-count partial-count) x))))))

; now lets define Y and rename partial-count as f
     (define Y
       (lambda (co)
         ((lambda (f)
            (f f))
          (lambda (f)
            (co (lambda (x) ((f f) x)))))))

; That is the Y Combinator!!!!

; this is the last bit missing !!!
(f f) == (co (lambda (x) ((f f) x))))

