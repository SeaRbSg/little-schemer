#lang racket
(require rackunit)

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (or (eq? a (car lat))
               (member? a (cdr lat)))])))

(test-case "member?"
           [check-true (member? 'sardines '(Italian sardines spaghetti parsley))])

(define is-first? 
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (eq? (car lat) a)])))

;; First definition 
;; (define two-in-a-row? 
;;   (lambda (lat)
;;     (cond
;;      [(null? lat) #f]
;;      [else (or (is-first? (car lat) (cdr lat)) 
;;                (two-in-a-row? (cdr lat)))])))

(define is-first-b?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (two-in-a-row? lat)])))

;; Second definition
;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond 
;;      [(null? lat) #f]
;;      [else (is-first-b? (car lat) (cdr lat))])))

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond
     [(null? lat) #f]
     [else (or (eq? (car lat) preceeding) 
               (two-in-a-row-b? (car lat) (cdr lat)))])))

(define two-in-a-row?
  (lambda (lat)
    (cond
     [(null? lat) #f]
     [else (two-in-a-row-b? (car lat) (cdr lat))])))

(test-case "two-in-a-row?"
           [check-false (two-in-a-row? '(Italian sardines spaghetti parsley))]
           [check-true (two-in-a-row? '(Italian sardines sardines spaghetti parsley))]
           [check-false (two-in-a-row? '(Italian sardines more sardines spaghetti))]
           [check-true (two-in-a-row? '(b d e i i a g))])


(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     [(null? tup) '()]
     [else (cons (+ sonssf (car tup))
                 (sum-of-prefixes-b 
                  (+ sonssf (car tup))
                  (cdr tup)))])))

(test-case "sum-of-prefixes-b"
           [check-equal? (sum-of-prefixes-b 0 '(1 1 1)) '(1 2 3)])

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(test-case "sum-of-prefixes"
           [check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29)]
           [check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5)]
           [check-equal? (sum-of-prefixes '()) '()])

(define pick
  (lambda (n lat)
    (cond
     [(eq? n 1) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

(test-case "pick"
           [check-equal? (pick 4 '(4 3 1 1 1)) 1]
           [check-equal? (pick 2 '(2 4 3 1 1 1)) 4])


(define scramble-b
  (lambda (tup rev-pre)
    (cond
     [(null? tup) '()]
     [else
      (cons (pick (car tup)
                  (cons (car tup) rev-pre))
            (scramble-b (cdr tup)
                        (cons (car tup) rev-pre)))])))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(test-case "scramble"
           [check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9)]
           [check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1)]
           [check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)])


