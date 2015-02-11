#lang racket/base

(require "lib/shared.rkt")

(require "ch03.rkt")                    ; multirember & firsts
(require "ch04.rkt")                    ; eqan? ** pick div
(require "ch06.rkt")                    ; operator 1st-sub-exp 2nd-sub-exp
(require "ch07.rkt")                    ; build first second a-pair? revpair

(module+ test
  (require rackunit))

;;; Chapter 8
;; pg 125-126

(define rember-f1
  (lambda (test? s l)
    (cond
     [(null? l) '()]
     [(test? (car l) s) (cdr l)]
     [else (cons (car l)
                 (rember-f1 test? s (cdr l)))])))

(module+ test
  (check-equal? (rember-f1 = 5 '())
                '())
  (check-equal? (rember-f1 = 5 '(6 2 5 3))
                '(6 2 3))
  (check-equal? (rember-f1 eq? 'jelly '(jelly beans are good))
                '(beans are good))
  (check-equal? (rember-f1 equal? '(pop corn) '(lemonade (pop corn) and (cake)))
                '(lemonade and (cake))))

;; pg 127-129

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(module+ test
  (check-true  ((eq?-c 'salad) 'salad))
  (check-false ((eq?-c 'salad) 'pie)))

(define eq?-salad (eq?-c 'salad))

(module+ test
  (check-true  (eq?-salad 'salad))
  (check-false (eq?-salad 'pie)))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) '()]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l)
                   ((rember-f test?) a (cdr l)))]))))

(define rember-eq? (rember-f eq?))

(module+ test
  (check-equal? (rember-eq? 'tuna '())
                '())
  (check-equal? (rember-eq? 'tuna '(tuna salad is good))
                '(salad is good))
  (check-equal? (rember-eq? 'tuna '(tuna salad is good))
                '(salad is good))
  (check-equal? ((rember-f eq?) 'tuna
                 '(shrimp salad and tuna salad))
                '(shrimp salad and salad)))

;; pg 130-133

(module+ test
  (check-equal? '(equal? eqan? eqlist? eqpair?)
                ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
            [(test? (car lat) old) (cons new
                                         (cons old
                                               (cdr lat)))]
            [else (cons (car lat)
                        ((insertL-f test?) new old (cdr lat)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
            [(test? (car lat) old) (cons old
                                         (cons new
                                               (cdr lat)))]
            [else (cons (car lat)
                        ((insertR-f test?) new old (cdr lat)))]))))

(module+ test
  (check-equal? ((insertR-f eq?) 'z 'c '())
                '())
  (check-equal? ((insertR-f eq?) 'z 'c '(a b c d e))
                '(a b c z d e))
  (check-equal? ((insertR-f eq?) 'e 'd '(a b c d f g d h))
                '(a b c d e f g d h))
  (check-equal? ((insertL-f eq?) 'z 'c '())
                '())
  (check-equal? ((insertL-f eq?) 'z 'c '(a b c d e))
                '(a b z c d e))
  (check-equal? ((insertL-f eq?) 'e 'd '(a b c d f g d h))
                '(a b c e d f g d h)))

(define insertX-f
  (lambda (match!)
    (lambda (test?)
      (lambda (new old lat)
        (cond [(null? lat) '()]
              [(test? (car lat) old) (match! new old (cdr lat))]
              [else (cons (car lat)
                          (((insertX-f match!) test?) new old (cdr lat)))])))))

(define seqR (lambda (new old l) (cons old (cons new l))))
(define seqL (lambda (new old l) (cons new (cons old l))))

(define insertR-fm (insertX-f seqR))
(define insertL-fm (insertX-f seqL))

(module+ test
  (check-equal? ((insertR-fm eq?) 'z 'c '())
                '())
  (check-equal? ((insertR-fm eq?) 'z 'c '(a b c d e))
                '(a b c z d e))
  (check-equal? ((insertR-fm eq?) 'e 'd '(a b c d f g d h))
                '(a b c d e f g d h))
  (check-equal? ((insertL-fm eq?) 'z 'c '())
                '())
  (check-equal? ((insertL-fm eq?) 'z 'c '(a b c d e))
                '(a b z c d e))
  (check-equal? ((insertL-fm eq?) 'e 'd '(a b c d f g d h))
                '(a b c e d f g d h)))

;; pg 134-135

(define atom-to-function
  (lambda (x)
    (cond [(eq? x '+) +]
          [(eq? x '*) *]
          [else expt])))

(define value4
  (lambda (exp)
    (cond
     [(atom? exp) exp]
     [else
      ((atom-to-function (operator exp))
       (value4 (1st-sub-exp exp))
       (value4 (2nd-sub-exp exp)))])))

(module+ test
  (check-true (eq? 4  (value4 '(+ 1 3))))
  (check-true (eq? 13 (value4 '(+ 1 (* 3 4)))))
  (check-true (eq? 9  (value4 '(+ 1 (^ 2 3))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a)
             ((multirember-f test?) a (cdr lat))]
            [else (cons (car lat)
                        ((multirember-f test?) a (cdr lat)))]))))

(module+ test
  (check-equal? '(a c d e) ((multirember-f equal?) 'b '(b a b c b d b e b))))

;; pg 137

(define multirember&co
  (lambda (a lat col)
    (cond [(null? lat) (col null null)]
          [(eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat (cons (car lat)
                                               seen))))]
          [else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat)
                                        newlat)
                                  seen)))])))

;; pg 138

(define a-friend (lambda (x y) (null? y)))

(module+ test
  (check-true  (multirember&co 'tuna '()                                a-friend))
  (check-false (multirember&co 'tuna '(tuna)                            a-friend))
  (check-false (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)))

;; pg 141

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat))))]
          [(eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat))))]
          [else (cons (car lat)
                      (multiinsertLR new oldL oldR (cdr lat)))])))

(module+ test
  (check-equal? (multiinsertLR 'a 'b 'c '())      '())
  (check-equal? (multiinsertLR 'a 'b 'c '(b))     '(a b))
  (check-equal? (multiinsertLR 'a 'b 'c '(c))     '(c a))
  (check-equal? (multiinsertLR 'a 'b 'c '(d e f)) '(d e f)))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond [(null? lat) (col '() 0 0)]
          [(eq? (car lat) oldL)
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons new (cons oldL newlat))
                                    (add1 L) R)))]
          [(eq? (car lat) oldR)
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons oldR (cons new newlat))
                                    L (add1 R))))]
          [else
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat)
                                    L R)))])))

(module+ test
  (check-equal? (multiinsertLR&co 'salty 'fish 'chips
                                  '(chips and fish or fish and chips)
                                  (lambda (newlat L R)
                                    (cons L (cons R newlat))))
                '(2 2 chips salty and salty fish or salty fish and chips salty))
  )

;; I don't get where this is going at all... I'm gonna try to go
;; faster to get to the next section.

;; pg 144

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(module+ test
  (check-false (even? 3))
  (check-true  (even? 4)))

(define evens-only*
  (lambda (l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(even? (car l)) (cons (car l)
                                        (evens-only* (cdr l)))]
                 [else (evens-only* (cdr l))])]
          [else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))])))

(module+ test
  (check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
                '((2 8) 10 (() 6) 2)))

;; fuck it... moving on to the next section.

(define keep-looking
  (lambda (a sorn lat)
    (cond [(number? sorn)
           (keep-looking a (pick sorn lat) lat)]
          [else (eq? sorn a)])))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(module+ test
  (check-true  (looking 'caviar '(6 2 4 caviar 5 7 3)))
  (check-false (looking 'caviar '(6 2 grits caviar 5 7 3))))

;; pg 151

;; (define eternity
;;   (lambda (x) (eternity x)))

;; pg 152

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(module+ test
  (check-equal? (shift '((a b) c))
                '(a (b c)))
  (check-equal? (shift '((a b) (c d)))
                '(a (b (c d)))))

(define align
  (lambda (pora)
    (cond [(atom? pora) pora]
          [(a-pair? (first pora))
           (align (shift pora))]
          [else (build (first pora)
                       (align (second pora)))])))

(module+ test
  (check-equal? (align 42) 42)
  ;; ugh
  )

(define length*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else (+ (length* (first pora))
                   (length* (second pora)))])))

(module+ test
  (check-equal? (length* '(1 2))
                2)
  (check-equal? (length* '(1 (2 3)))
                3)
  (check-equal? (length* '((1 2) (3 4)))
                4)
  (check-equal? (length* '((1 2 3) (4 5 6)))
                4)) ; seems useless

;; pg 154

(define weight*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else (+ (* (weight* (first pora)) 2)
                   (weight* (second pora)))])))

(module+ test
  (check-equal? (weight* '((a b) c))
                7)
  (check-equal? (weight* '(a (b c)))
                5))

(define shuffle
  (lambda (pora)
    (cond [(atom? pora) pora]
          [(a-pair? (first pora))
           (shuffle (revpair pora))]
          [else (build (first pora)
                       (shuffle (second pora)))])))

(module+ test
  (check-equal? (shuffle '(a (b c))) '(a (b c)))
  (check-equal? (shuffle '(a b)) '(a b))
  (check-equal? (revpair '(a b)) '(b a)))

(define C
  (lambda (n)
    (cond [(= 1 n) 1]
          [else (cond [(even? n) (C (div n 2))]
                      [else (C (add1 (* 3 n)))])])))

(module+ test
  (check-equal? (C 1) 1)
  (check-equal? (C 2) 1)
  (check-equal? (C 3) 1)
  (check-equal? (C 4) 1))

;; pg 156

(define A
  (lambda (n m)
    (cond [(zero? n) (add1 m)]
          [(zero? m) (A (sub1 n) 1)]
          [else (A (sub1 n) (A n (sub1 m)))])))

(module+ test
  (check-equal? (A 1 0) 2)
  (check-equal? (A 1 1) 3)
  (check-equal? (A 2 2) 7))
