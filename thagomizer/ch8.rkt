#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; Previous Chapters
(define o+
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (add1 (o+ x (sub1 y)))])))

(define ×
  (lambda (m n)
    (cond
     [(zero? n) 0]
     [else (o+ m (× m (sub1 n)))])))

(define ↑
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (× n (↑ n (sub1 m)))])))

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))



;; Ch 8
(define rember-f
  (lambda (test? a l)
    (cond 
     [(null? l) l]
     [(test? a (car l)) (cdr l)]
     [else (cons (car l) (rember-f test? a (cdr l)))])))

(test-case "rember-f"
           [check-equal? (rember-f = 5 '(6 2 5 3)) '(6 2 3)]
           [check-equal? (rember-f eq? 'jelly '(jelly beans are good)) '(beans are good)]
           [check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake))])


(define eq-to-arg
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-c eq-to-arg)

(define eq-to-salad
  (eq-to-arg 'salad))

(test-case "eq-to-salad"
           [check-true (eq-to-salad 'salad)]
           [check-false (eq-to-salad 'tuna)])

(define rember-f-curry
  (lambda (test?)
    (lambda (a l)
      (cond 
       [(null? l) l]
       [(test? a (car l)) (cdr l)]
       [else (cons (car l) ((rember-f-curry test?) a (cdr l)))]))))

(define rember-eq? (rember-f-curry eq?))

(test-case "rember-eq?"
           [check-equal? (rember-eq? 'tuna '(tuna salad is good)) '(salad is good)])

(test-case "rember-f-curry"
           [check-equal? ((rember-f-curry eq?) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad)]
           [check-equal? ((rember-f-curry eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?)])

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) l]
       [(test? old (car l)) (cons new l)]
       [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

(test-case "insertL-f"
           [check-equal? ((insertL-f eq?) 'a 'b '()) '()]
           [check-equal? ((insertL-f eq?) 'a 'b '(c d)) '(c d)]
           [check-equal? ((insertL-f eq?) 't 'f '(i c w f d)) '(i c w t f d)]
           [check-equal? ((insertL-f equal?) '(a b) '(x y) '(t (u) w (x y)))
                         '(t (u) w (a b) (x y))])
           

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) l]
       [(test? old (car l)) (cons (car l) (cons new (cdr l)))]
       [else (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

(test-case "insertR-f"
           [check-equal? ((insertR-f eq?) 'a 'b '()) '()]
           [check-equal? ((insertR-f eq?) 'a 'b '(c d)) '(c d)]
           [check-equal? ((insertR-f eq?) 't 'f '(i c w f d)) '(i c w f t d)]
           [check-equal? ((insertR-f equal?) '(a b) '(x y) '(t (u) w (x y)))
                         '(t (u) w (x y) (a b))])


(define insert-g-f
  (lambda (direction test?)
    (lambda (new old l)
      (cond
       [(null? l) l]
       [(test? old (car l))
        (cond 
         [(eq? direction 'l) (cons new l)]
         [else (cons (car l) (cons new (cdr l)))])]
       [else (cons (car l) ((insert-g-f direction test?) new old (cdr l)))]))))

(test-case "insert-g-f"
           [check-equal? ((insert-g-f 'r eq?) 'a 'b '()) '()]
           [check-equal? ((insert-g-f 'r eq?) 'a 'b '(c d)) '(c d)]
           [check-equal? ((insert-g-f 'r eq?) 't 'f '(i c w f d)) '(i c w f t d)]
           [check-equal? ((insert-g-f 'r equal?) '(a b) '(x y) '(t (u) w (x y)))
                         '(t (u) w (x y) (a b))]
           [check-equal? ((insert-g-f 'l eq?) 'a 'b '()) '()]
           [check-equal? ((insert-g-f 'l eq?) 'a 'b '(c d)) '(c d)]
           [check-equal? ((insert-g-f 'l eq?) 't 'f '(i c w f d)) '(i c w t f d)]
           [check-equal? ((insert-g-f 'l equal?) '(a b) '(x y) '(t (u) w (x y)))
                         '(t (u) w (a b) (x y))])


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       [(null? l) l]
       [(eq? (car l) old) (seq new old (cdr l))]
       [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(test-case "insertL"
           [check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with topping fudge for dessert)]
           [check-equal? (insertL 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales jalapeño and salsa)]
           [check-equal? (insertL 'e 'd '(a b c d f g d h))
                         '(a b c e d f g d h)])

(test-case "insertR"
           [check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with fudge topping for dessert)]
           [check-equal? (insertR 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales and jalapeño salsa)]
           [check-equal? (insertR 'e 'd '(a b c d f g d h))
                         '(a b c d e f g d h)])


(define insertL2 
  (insert-g 
   (lambda (new old l) (cons new (cons old l)))))

(define insertR2
  (insert-g
   (lambda (new old l) (cons old (cons new l)))))

(test-case "insertL2"
           [check-equal? (insertL2 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with topping fudge for dessert)]
           [check-equal? (insertL2 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales jalapeño and salsa)]
           [check-equal? (insertL2 'e 'd '(a b c d f g d h))
                         '(a b c e d f g d h)])

(test-case "insertR2"
           [check-equal? (insertR2 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with fudge topping for dessert)]
           [check-equal? (insertR2 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales and jalapeño salsa)]
           [check-equal? (insertR2 'e 'd '(a b c d f g d h))
                         '(a b c d e f g d h)])

;; ARGH...why is are we passing the cdr l instead of just passing l and do the 
;; cdr-ing here
(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond 
     [(eq? x '+) o+]
     [(eq? x '×) ×]
     [else ↑])))

(test-case "atom-to-function"
           [check-eq? (atom-to-function (operator '(+ 5 3))) o+])

(define value
  (lambda (nexp)
    (cond
     [(and (atom? nexp) (number? nexp)) nexp]
     [else ((atom-to-function (operator nexp)) 
            (value (1st-sub-exp nexp)) 
            (value (2nd-sub-exp nexp)))])))

(test-case "value"
           [check-eq? (value '13) 13]
           [check-eq? (value '(+ 1 3)) 4])


(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) l]
       [(test? (car l) a) ((multirember-f test?) a (cdr l))]
       [else (cons (car l) ((multirember-f test?) a (cdr l)))]))))

(test-case "multirember-f"
           [check-equal? ((multirember-f eq?) 'a '()) '()]
           [check-equal? ((multirember-f eq?) 'mint '(lamb chops and mint jelly))
                      '(lamb chops and jelly)]
           [check-equal? ((multirember-f eq?) 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea and hick)]
           [check-equal? ((multirember-f eq?) 'bacon '(bacon lettuce and tomato))
                      '(lettuce and tomato)]
           [check-equal? ((multirember-f equal?) '(a b) '(x (a b) y (a b) a b))
                         '(x y a b)]
           [check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)])

(define multirember-eq? 
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
 (lambda (test?)
   (lambda (l)
     (cond
      [(null? l) l]
      [(test? (car l)) ((multiremberT test?) (cdr l))]
      [else (cons (car l) ((multiremberT test?) (cdr l)))]))))

(test-case "multiremberT"
           [check-equal? ((multiremberT eq?-tuna) '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)])


(define multirember&co
  (lambda (a lat col)
    (cond
     [(null? lat) 
      (col '() '())]
     [(eq? (car lat) a) 
      (multirember&co a 
                      (cdr lat) 
                      (lambda (newlat seen) 
                        (col newlat (cons (car lat) seen))))]
     [else (multirember&co a 
                           (cdr lat) 
                           (lambda (newlat seen) 
                             (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend (lambda (x y) (length x)))

(test-case "multirember&co"
          [check-equal? (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend) #f]
          [check-equal? (multirember&co 'tuna '() a-friend) #t]
          [check-equal? (multirember&co 'tuna '(tuna) a-friend) #f]
          [check-equal? (multirember&co 'tuna '(and tuna) a-friend) #f]
          [check-equal? (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend) 3])

;; (define new-friend
;;   (lambda (newlat seen)
;;     (col newlat (cons (car lat) seen))))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond 
     [(null? lat) '()]
     [(eq? oldL (car lat))
      (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
     [(eq? oldR (car lat))
      (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
     [else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))


(test-case "multiinsertLR"
           [check-equal? (multiinsertLR 'a '() 'b '()) '()]
           [check-equal? (multiinsertLR 'x '() 'a '(a b a c d)) '(a x b a x c d)]
           [check-equal? (multiinsertLR 'x '() 'a '(b a c a d)) '(b a x c a x d)]
           [check-equal? (multiinsertLR 'a 'b '() '()) '()]
           [check-equal? (multiinsertLR 'x 'a '() '(a b a c d)) '(x a b x a c d)]
           [check-equal? (multiinsertLR 'x 'a '() '(b a c a d)) '(b x a c x a d)]
           [check-equal? (multiinsertLR 'fried 'fish '() '(chips and fish or fish and fried)) '(chips and fried fish or fried fish and fried)])


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     [(null? lat) (col '() 0 0)]
     [(eq? oldL (car lat))
      (multiinsertLR&co new 
                        oldL
                        oldR
                        (cdr lat)
                        (lambda (newlat L R) 
                          (col (cons new 
                                     (cons oldL newlat)) (add1 L) R)))]
     [(eq? oldR (car lat))
      (multiinsertLR&co new
                        oldL
                        oldR
                        (cdr lat)
                        (lambda (newlat L R) 
                          (col (cons oldR 
                                     (cons new newlat)) L (add1 R))))]
     [else (multiinsertLR&co new 
                             oldL 
                             oldR 
                             (cdr lat)
                             (lambda (newlat L R) 
                               (col (cons (car lat) newlat) L R)))])))


(define evens-only*
  (lambda (l)
    (cond
     [(null? l) l]
     [(not (atom? (car l))) (cons (evens-only* (car l)) (evens-only* (cdr l)))]
     [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
     [else (evens-only* (cdr l))])))

(test-case "evens-only*"
           [check-equal? (evens-only* '(1 2 3)) '(2)]
           [check-equal? (evens-only* '(1 (1 2) 3 (5 8) 13 21 34))
                         '((2) (8) 34)]
           [check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
                         '((2 8) 10 (()  6) 2)])

(define evens-only*&co
  (lambda (l col)
    (cond
     [(null? l) (col l 1 0)] ;; 1 is the multiplicative identity, 0 for adding
     [(not (atom? (car l))) 
      (evens-only*&co (car l) 
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (* ap dp)
                                               (+ as ds))))))]
     [(even? (car l))
      (evens-only*&co (cdr l)
                      (lambda (newl p s)
                        (col (cons (car l) newl)
                             (* (car l) p) s)))]
     [else (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl
                                  p (+ (car l) s))))])))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))

(test-case "evens-only*&co"
           [check-equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                                        the-last-friend) '(38 1920 (2 8) 10 (() 6) 2)])
