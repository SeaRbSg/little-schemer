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

;;;;; BREATHE ;;;;;

; write rember for atoms

(define rember-atom
  (lambda (a lat)
    (cond
      [(null? lat) lat]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember-atom a (cdr lat)))])))

(module+ test
  [check-equal? (rember-atom 'a '(1 2 a 3 a 4 a a)) '(1 2 3 a 4 a a)])

; ...generalize for all S-expressions by passing the testing function as argument

(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) lat]
        [(test? a (car lat)) (cdr lat)]
        [else (cons (car lat) ((rember-f test?) a (cdr lat)))]))))

(module+ test
  [check-equal? ((rember-f eq?) '1 '(1 2 3 4 1)) '(2 3 4 1)])

; ...do the same for multirember

(define my-multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) lat]
        [(test? a (car lat)) ((my-multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((my-multirember-f test?) a (cdr lat)))]))))


(module+ test
  [check-equal? ((my-multirember-f eq?) 'a '(v a c a w a)) '(v c w)]
  [check-equal? ((my-multirember-f eq?) 'sardines '(sardines)) '()])

; can we letreccify it (considering that (my-multirember-f test?) doesn't change)
; very easy, just remember that the letrec returns a value, which can be a function
; that abbreviates our call (my-multirember-f test?)

(define my-multirember-f2
  (lambda (test?)
    (letrec
        ((m-f (lambda (a lat)
                 (cond
                   [(null? lat) lat]
                   [(test? a (car lat)) (m-f a (cdr lat))]
                   [else (cons (car lat) (m-f a (cdr lat)))]))))
       m-f)))


(module+ test
  [check-equal? ((my-multirember-f2 eq?) 'a '(v a c a w a)) '(v c w)]
  [check-equal? ((my-multirember-f2 eq?) 'sardines '(sardines)) '()])

; another
; write member?

(define my-member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? a (car lat)) #t]
      [else (my-member? a (cdr lat))])))

; a does not change => letreccify

(define member-v1?
  (lambda (a lat)
    (letrec
        ((includes? (lambda (l)
                      (cond
                        [(null? l) #f]
                        [(eq? a (car l)) #t]
                        [else (includes? (cdr l))]))))
      (includes? lat))))

; write it too with in ((letrec format

(define member-v2?
  (lambda (a lat)
    ((letrec
        ((includes? (lambda (l)
                      (cond
                        [(null? l) #f]
                        [(eq? a (car l)) #t]
                        [else (includes? (cdr l))]))))
      includes?)
     lat)))

(module+ test
  [check-true (member-v1? '1 '(9 2 3 1 0))]
  [check-true (member-v2? '1 '(9 2 3 1 0))])

;;;;; BREATHE ;;;;;

; another
; write union of sets

(define my-union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member-v1? (car s1) s2) (my-union (cdr s1) s2)]
      [else (my-union (cdr s1) (cons (car s1) s2))])))

; no, not that way, the other dyslexic way!

(define my-union-v2
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member-v1? (car s1) s2) (my-union-v2 (cdr s1) s2)]
      [else (cons (car s1) (my-union-v2 (cdr s1) s2))])))

(module+ test
  [check-equal? (my-union '(1 2 3 4) '(2 4 6 8)) '(3 1 2 4 6 8)]
  [check-equal? (my-union-v2 '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8)])

; in this version s2 never changes (we just use it to compare) => letreccify

(define union-v1
  (lambda (s1 s2)
    (letrec
        ((+ (lambda (set)
              (cond
                [(null? set) s2]
                [(member-v2? (car set) s2) (+ (cdr set))]
                [else (cons (car set) (+ (cdr set)))]))))
      (+ s1))))

(define union-v2
  (lambda (s1 s2)
    ((letrec
        ((+ (lambda (set)
              (cond
                [(null? set) s2]
                [(member-v2? (car set) s2) (+ (cdr set))]
                [else (cons (car set) (+ (cdr set)))]))))
       +)
     s1)))

(module+ test
  [check-equal? (union-v1 '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8)]
  [check-equal? (union-v2 '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8)])

; outside
; (+ 1 2) => 3
; (+ '(1 2) '(3 4)) => error - not numbers
; This means that the (+ defined above only exists inside that union-v1 lambda

; for completly different...
; since member-v2? is references inside the lambda => this is a potential for mischieve
; push it inside the letrec too with double parens (easy)

(define union-v3
  (lambda (s1 s2)
    (letrec
        ((+ (lambda (set)
              (cond
                [(null? set) s2]
                [(in? (car set) s2) (+ (cdr set))]
                [else (cons (car set) (+ (cdr set)))])))
         (in? (lambda (a l)
              (cond
                [(null? l) #f]
                [(eq? a (car l)) #t]
                [else (in? a (cdr l))])))
         )
      (+ s1))))

(module+ test
  [check-equal? (union-v3 '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8)])

; THE THIRTEEN COMMANDMENT: Letreccify to hide and protect functions

; but we have not finished with union because the fucking elephant is never happy!!
; let's get rid of the in? call to a fixed 'a

(define union-v4
  (lambda (s1 s2)
    (letrec
        ((+ (lambda (set)
              (cond
                [(null? set) s2]
                [(in? (car set) s2) (+ (cdr set))]
                [else (cons (car set) (+ (cdr set)))])))
         (in? 
          (lambda (a lat)
            (letrec 
                ((? (lambda (lali)
                     (cond
                       [(null? lali) #f]
                       [(eq? a (car lali)) #t]
                       [else (? (cdr lali))]))))
              (? lat))))
         )
      (+ s1))))

(module+ test
  [check-equal? (union-v4 '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8)])

;;;;; BREATHE ;;;;;

; Let's go now to te way we defined functions in Ch 11. Start copying two-in-a-row

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      [(null? lat) #f]
      [else
       (or (eq? preceding (car lat))
       (two-in-a-row-b? (car lat) (cdr lat)))])))

(define two-in-a-row-a?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat) (cdr lat))])))

(module+ test
  [check-true (two-in-a-row-a? '(b c a a s e))])

; letreccify

(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((æ? (lambda (pre l)
               (cond
                 [(null? l) #f]
                 [else
                  (or (eq? pre (car l))
                      (æ? (car l) (cdr l)))]))))
      (cond                                         ; \
        [(null? lat) #f]                            ;  | All this is the value returned by the letrec!!!
        [else (æ? (car lat) (cdr lat))]))))         ; /

(module+ test
  [check-true (two-in-a-row? '(b c a a s e))])

; beautiful, now chapter 11 makes sense. We can create local functions inside the scope of more publicly
; available ones and minimize complexity.

; we can re-assemble lines

(define two-in-a-row-v2?
  (letrec
      ((æ? (lambda (pre l)
             (cond
               [(null? l) #f]
               [else
                (or (eq? pre (car l))
                    (æ? (car l) (cdr l)))]))))
    (lambda (lat)
      (cond                                         ; \
        [(null? lat) #f]                            ;  | All this is the value returned by the letrec!!!
        [else (æ? (car lat) (cdr lat))]))))         ; /

(module+ test
  [check-true (two-in-a-row-v2? '(b c a a s e))])

; protect sum-of-prefixes

(define sum-of-prefixes
  (letrec
      ((Σ (lambda (accsum tup)
            (cond
              [(null? tup) tup]
              [else (cons (+ (car tup) accsum)
                          (Σ (+ (car tup) accsum) (cdr tup)))]))))
    (lambda (tup)
      (Σ 0 tup))))

(module+ test
  (check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5)))

; and finally, scramble!

(define scramble-v1
  (letrec
      ((pick (lambda (n lat)
            (cond
              [(eq? n '1) (car lat)]
              [else (pick (- n 1) (cdr lat))])))
       (experiment (lambda (wtf lat)
            (cond
              ((null? lat) lat)
              (else 
               (cons (pick (car lat) (cons (car lat) wtf))
                     (experiment (cons (car lat) wtf) (cdr lat))))))))
    (lambda (lat)
      (experiment '() lat))))

(module+ test
  (check-equal? (scramble-v1 '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (scramble-v1 '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (scramble-v1 '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

; it would be better with pick inside experiment


(define scramble-v2
  (letrec
      ((X
        (letrec
            ((pick (lambda (n l)
                     (cond
                       [(eq? n '1) (car l)]
                       [else (pick (- n 1) (cdr l))]))))
          (lambda (wtf l)
            (cond
              ((null? l) l)
              (else (and
                     (cons (pick (car l) (cons (car l) wtf))
                           (X (cons (car l) wtf) (cdr l))))))))))
    (lambda (lat)
      (X '() lat))))

(module+ test
  (check-equal? (scramble-v2 '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (scramble-v2 '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (scramble-v2 '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

; now since I used ch11 to code game of life, all is left to do is to letreccify GoL
