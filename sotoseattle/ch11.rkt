#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require racket/trace)

; let's get reacquainted wityh member?!

(define member_long?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else 
       (cond
         [(eq? a (car lat)) #t]
         [else (member_long? a (cdr lat))])])))

; That is the long form, but eq? already gives us #t ot #f

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else 
       (or
         (eq? a (car lat))
         (member? a (cdr lat)))])))

(module+ test
  [check-true (member_long? 'a '(v c a w))]
  [check-true (member? 'a '(v c a w))]
  [check-true (member? 'sardines '(Italian sardines spaghetti parsley))])

; Try writing two-in-a-row

(define my-two-in-a-row?
  (lambda (lat)
    (cond
      [(or (null? lat) (null? (cdr lat))) #f]
      [(eq? (car lat) (car (cdr lat))) #t]
      [else (my-two-in-a-row? (cdr lat))])))

(module+ test
  [check-true (my-two-in-a-row? '(b c a a s e))]
  [check-true (my-two-in-a-row? '(a a))]
  [check-true (my-two-in-a-row? '(a a b))]
  [check-false (my-two-in-a-row? '(b c a d a e))]
  [check-false (my-two-in-a-row? '(b c b c b c))]
  [check-false (my-two-in-a-row? '(b c))]
  [check-false (my-two-in-a-row? '(b))]
  [check-false (my-two-in-a-row? '())])

; Let's follow the book

(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? a (car lat))])))

; Given...

(define two-in-a-row-with_first?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (is-first-b? (car lat) (cdr lat))])))

; ...write is-first-b?

(define is-first-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat))
             (two-in-a-row-with_first? (cdr lat)))])))

(module+ test
  [check-true (two-in-a-row-with_first? '(b c a a s e))]
  [check-true (two-in-a-row-with_first? '(a a))]
  [check-true (two-in-a-row-with_first? '(a a b))]
  [check-false (two-in-a-row-with_first? '(b c a d a e))]
  [check-false (two-in-a-row-with_first? '(b c b c b c))]
  [check-false (two-in-a-row-with_first? '(b c))]
  [check-false (two-in-a-row-with_first? '(b))]
  [check-false (two-in-a-row-with_first? '())])

; So two-in-a-row calls is-first-b, who calls back two-in-a-row with different arguments
; What a convoluted way to do things, but we can fudge it even more:

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      [(null? lat) #f]
      [else
       (or (eq? preceding (car lat))
       (two-in-a-row-b? (car lat) (cdr lat)))])))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat) (cdr lat))])))

(module+ test
  [check-true (two-in-a-row? '(b c a a s e))]
  [check-true (two-in-a-row? '(a a))]
  [check-true (two-in-a-row? '(a a b))]
  [check-false (two-in-a-row? '(b c a d a e))]
  [check-false (two-in-a-row? '(b c b c b c))]
  [check-false (two-in-a-row? '(b c))]
  [check-false (two-in-a-row? '(b))]
  [check-false (two-in-a-row? '())])

; Although my-two-in-a-row may be clearer, I think that the idea is that we can define
; internal helper functions with extra arguments that helps us code the problem.

; Let's go for another. It serves both as a review of little schemer basics and to introduce
; the 11th Commandment

(define sum-of-prefixes-b
  (lambda (accsum tup)
    (cond
      [(null? tup) tup]
      [else (cons (+ (car tup) accsum)
                  (sum-of-prefixes-b (+ (car tup) accsum)
                                     (cdr tup)))])))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(module+ test
  (check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5)))

; Last one. Given pick (from before) try to write your own scramble function

(define pick
  (lambda (n lat)
    (cond
      [(eq? n '1) (car lat)]
      [else (pick (- n 1) (cdr lat))])))

(module+ test
  (check-equal? (pick 4 '(4 3 1 1 1)) 1)
  (check-equal? (pick 2 '(2 4 3 1 1)) 4))

(define experiment
  (lambda (wtf lat)
    (cond
      ((null? lat) lat)
      (else (and
             ;(print (cons (car lat) wtf))
             ;(print (pick (car lat) (cons (car lat) wtf)))
             (cons (pick (car lat) (cons (car lat) wtf))
                   (experiment (cons (car lat) wtf) (cdr lat))))))))
             ;(experiment (cons (car lat) wtf) (cdr lat)))))))

(module+ test
  (check-equal? (experiment '() '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (experiment '() '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (experiment '() '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))
  
(define scramble
  (lambda (lat)
    (experiment '() lat)))

(module+ test
  (check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (scramble '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

; although I was able to write experiment (helping myself with print) I still need to review
; and see if I can do it differently.

; this chapter is easier than I thought and more like a review of recursion concepts from the previous book.
