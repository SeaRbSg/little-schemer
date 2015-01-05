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

; dont even try this at home
; (will-stop? (last-try '()))

; will-stop? is a function that we can define in our minds, that we can express with words,
; but that we can NOT define with Scheme <== define

; the rest of the chapter is the derivation of the Y combinator
; no need to code it here, but to follow with paper and pen
; another good input for this: http://mvanier.livejournal.com/2897.html

  
  