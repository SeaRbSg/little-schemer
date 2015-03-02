#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch04.rkt")
(require "ch05.rkt")

(define (leftmost* l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (car l)]
    [else
      (let ([a (leftmost* (car l))])
        (cond
          [(atom? a) a]
          [else (leftmost* (cdr l))]))]))

(define (test-leftmost* leftmost*)
  (test-case "leftmost*"
     (check-equal? (leftmost* '(((a) b) (c d))) 'a)
     (check-equal? (leftmost* '(((a) ()) () (e))) 'a)
     (check-equal? (leftmost* '(((()) a) ())) 'a)
     (check-equal? (leftmost* '(((a)) b (c))) 'a)))

(test-leftmost* leftmost*)

(define (rember1* a l)
  (letrec
    ([R (lambda (l)
          (cond
            [(null? l) '()]
            [(atom? (car l))
             (cond
               [(eq? (car l) a) (cdr l)]
               [else (cons (car l) (R (cdr l)))])]
            [else
              (let ([av (R (car l))])
                (cond
                  [(eqlist? av (car l))
                   (cons (car l) (R (cdr l)))]
                  [else (cons av (cdr l))]))]))])
    (R l)))

(define (testcase-rember1* rember1*)
  (test-case "rember1*"
     (check-equal? (rember1* 'salad '((Swedish rye)
                                      (French (mustard salad turkey))
                                      salad))
                   '((Swedish rye)
                     (French (mustard turkey))
                     salad))

     (check-equal? (rember1* 'meat '((pasta meat)
                                     pasta
                                     (noodles meat sauce)
                                     meat tomatoes))
                   '((pasta)
                     pasta
                     (noodles meat sauce)
                     meat tomatoes))))

(testcase-rember1* rember1*)

(define (max n m)
  (if (> n m) n m))

; shortest version is best version ^_^
(define (depth* l)
  (cond
    [(null? l) 1]
    [(atom? (car l)) (depth* (cdr l))]
    [else
      (max (add1 (depth* (car l)))
           (depth* (cdr l)))]))

(test-case "depth*"
    (check-equal? (depth* '((picked) peppers (peppers picked))) 2)
    (check-equal? (depth* '(margarine
                            ((bitter butter)
                             (makes)
                             (batter (bitter)))
                            butter)) 4)
    (check-equal? (depth* '(c (b (a b) a) a)) 3))

; perfect scrambled eggs
(define (scramble tup)
  (letrec
    ([P (lambda (tup rev-pre)
          (cond
            [(null? tup) '()]
            [else
              (let ([rp (cons (car tup) rev-pre)])
                (cons (pick (car tup) rp)
                      (P (cdr tup) rp)))]))])
    (P tup '())))

(test-case "scamble"
    (check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
    (check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1))
    (check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

(define (leftmost2* l)
  (let/cc skip
      (letrec
        ([lm (lambda (l)
               (cond
                 [(null? l) '()]
                 [(atom? (car l)) (skip (car l))]
                 [else
                   (begin
                     (lm (car l))
                     (lm (cdr l)))]))])
        (lm l))))

(test-leftmost* leftmost2*)

; try from book - didn't work :(

; (define (try x a b)
;   (let/cc success
;       (let/cc x
;           (success a))
;       b))

; Try has to be defined with macros
(define-syntax try
  (syntax-rules ()
    ((try var a b)
     (let/cc success
        (let/cc var (success a)) b))))

; originally from https://github.com/viswanathgs/The-Seasoned-Schemer/blob/master/ch-14-let-there-be-names.ss#L422

(define (rm a l oh)
  (cond
    [(null? l) (oh 'no)]
    [(atom? (car l))
     (if (eq? (car l) a)
       (cdr l)
       (cons (car l) (rm a (cdr l) oh)))]
    [else
      (try oh2
        (cons (rm a (car l) oh2) (cdr l))
        (cons (car l) (rm a (cdr l) oh)))]))

(test-case "rm"
    (check-equal? (let/cc Say (rm 'noodles '((food) more (food)) Say)) 'no))

(define (rember1*-2 a l)
  (let ([new-l (let/cc oh (rm a l oh))])
    (if (atom? new-l)
      l
      new-l)))

(testcase-rember1* rember1*-2)

(define (rember1*-3 a l)
  (try oh (rm a l oh) l))

(testcase-rember1* rember1*-3)
