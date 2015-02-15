#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; Previous Chapters
(define pick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))


;; Ch 9

(define keep-looking
  (lambda (a s-or-n lat)
    (cond 
     [(number? s-or-n) 
      (keep-looking a (pick s-or-n lat) lat)]
     [else (eq? s-or-n a)])))


(test-case "keep-looking"
           [check-true (keep-looking 'caviar 3 '(6 2 4 caviar 5 7 3))])

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(test-case "looking"
           [check-true (looking 'caviar '(6 2 4 caviar 5 7 3))]
           [check-false (looking 'caviar '(6 2 grits caviar 5 7 3))])

(test-case "pick"
           [check-eq? (pick 6 '(6 2 4 caviar 5 7 3)) 7])


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(test-case "shift"
           [check-equal? (shift '((a b) c)) '(a (b c))]
           [check-equal? (shift '((a b) (c d))) '(a (b (c d)))])


