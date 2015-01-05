#lang racket
(require "lib/shared.rkt")
(require "ch4.rkt")
(require "ch6.rkt")
(require rackunit)

; rember from ch 5
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) lat]
      [(eq? a (car lat)) (rember a (cdr lat))]
      [else (cons (car lat) (rember a (cdr lat)))])))

(module+ test (check-equal? (rember 'a '(b c a)) '(b c)))

; intuitively enough
(define rember-f
  (lambda (test? a lis)
    (cond
      [(null? lis) lis]
      [(test? a (car lis)) (rember-f test? a (cdr lis))]
      [else (cons (car lis) (rember-f test? a (cdr lis)))])))

(module+ test
  (check-equal? (rember-f = '5 '(6 2 5 3)) '(6 2 3))
  (check-equal? (rember-f eq? 'a '(b c a)) '(b c))
  (check-equal? (rember-f eq? 'jelly '(jelly beans are good)) '(beans are good))
  (check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; just another function where argument is a, but now it returns another function (with a set)
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(module+ test
  [check-true ((eq?-c 'salad) 'salad)]
  [check-false ((eq?-c 'salad) 'pepe)])

(define rember_f
  (lambda (test?)
    (lambda (a lis)
      (cond
        [(null? lis) lis]
        [(test? a (car lis))  ((rember_f test?) a (cdr lis))]
        [else (cons (car lis) ((rember_f test?) a (cdr lis)))]))))

(module+ test
  (check-equal? ((rember_f =) '5 '(6 2 5 3)) '(6 2 3))
  (check-equal? ((rember_f eq?) 'a '(b c a)) '(b c))
  (check-equal? ((rember_f eq?) 'jelly '(jelly beans are good)) '(beans are good))
  (check-equal? ((rember_f equal?) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))
  (check-equal? ((rember_f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?)))

; equal? works with lists // eq? with atoms // = with numbers
; A function that returns a function is what CoffeeScript does naturally and Ruby can do with Procs
;
; coffee_sqc = (a) -> (x) -> x == a
; coffee_sqc(1)(1) #=> true
;
; def ruby_eqc?(a)
;   -> (x) { x == a }
; end
; puts ruby_eqc?(true).call(true) #=> true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertL ; <--------------------- INSERT_L from ch 3
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons new (cons old (cdr lat)))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define insertR ; <--------------------- INSERT_R from ch 3
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(module+ test
  (check-equal? (insertL 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? (insertR 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? (insertR 9 1 '(1)) '(1 9)))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(test? old (car l)) (cons new (cons old (cdr l)))]
        [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(test? old (car l)) (cons old (cons new (cdr l)))]
        [else (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

(module+ test
  (check-equal? ((insertL-f eq?) 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? ((insertR-f eq?) 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? ((insertR-f eq?) 9 1 '(1)) '(1 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; so far so good. Now, since there is only one thing that changes we extract it
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; and rewrite a more abstract insert global
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) l]
        [(eq? old (car l)) (seq new old (cdr l))]
        [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

; and we can redefine insert L and R
(define insertL#
  (insert-g seqL))

(define insertR#
  (insert-g seqR))

(module+ test
  (check-equal? (insertL# 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? (insertR# 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? (insertR# 9 1 '(1)) '(1 9)))

; or we can re-contra-abstract-it-back
(define insertL_
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR_
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(module+ test
  (check-equal? (insertL_ 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
  (check-equal? (insertR_ 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
  (check-equal? (insertR_ 9 1 '(1)) '(1 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Substitute function from chapter 3
(define subst
  (lambda (new old l)
    (cond
      [(null? l) l]
      [(eq? old (car l)) (cons new (cdr l))]
      [else (cons (car l) (subst new old (cdr l)))])))

; we work the same as with insert L/R
(define seqS
  (lambda (new old l)
    (cons new l)))

(define substS
  (insert-g seqS))

(module+ test
  (check-equal? (substS 'x 2 '(1 2 3 4)) '(1 x 3 4))
  (check-equal? (substS 9 1 '(1)) '(9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; now in reverse, given the following two functions, what is yyy?
(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

; easy to get lost, so I'll walk it through
; seqrem is => give back just the list
; insert-g gets 3 args:
;   new  => false
;   old  => an atom
;   list => a list

; (define insert-g
;   (lambda (seqrem)
;     (lambda (#f a l)
;        (cond
;          [(null? l) l]
;          [(eq? a (car l)) (seqrem #f a (cdr l))] ; if the first element is a => give back the rest of the list
;          [else (cons (car l) ((insert-g seq) #f a (cdr l)))]))))
; ok, so it is like remove a from the list ==> REMBER!!

; QUESTION 1: So why use #f, and not #t or 'pepe? It seems to be irrelevant?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we get back the value function from ch6
(define val
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) '+) (o+ (val (1st-sub-exp nexp)) (val (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'x) (x  (val (1st-sub-exp nexp)) (val (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '^) (^  (val (1st-sub-exp nexp)) (val (2nd-sub-exp nexp)))])))

(module+ test
  (check-equal? (val '(+ 2 3)) 5)
  (check-equal? (val '(x 2 3)) 6)
  (check-equal? (val '(^ 2 3)) 8)
  (check-equal? (val '(+ 2 (+ 1 2))) 5))

(define atom_to_function
  (lambda (a)
    (cond
      [(eq? a '+) +]
      [(eq? a 'x) x]
      [else ^])))

(define val2
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom_to_function (operator nexp))(val2 (1st-sub-exp nexp)) (val2 (2nd-sub-exp nexp)))])))

(module+ test
  (check-equal? (val2 '(+ 2 3)) 5)
  (check-equal? (val2 '(x 2 3)) 6)
  (check-equal? (val2 '(^ 2 3)) 8)
  (check-equal? (val2 '(+ 2 (+ 1 2))) 5))

; atom_to_function and the refactor answers Question 2 in chapter 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; now back to multirember from chapter 3
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) lat]
      [(eq? a (car lat)) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

(define multirember-f ; easy peasy
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) lat]
        [(test? a (car lat))  ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))

(module+ test
  (check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)))

(define multirember-eq?
  (multirember-f eq?))

(define multirember-fa ; my way of combining a and test? (wrong obviously)
  (lambda (a test?)
    (lambda (lat)
      (cond
        [(null? lat) lat]
        [(test? a (car lat))  ((multirember-fa a test?) (cdr lat))]
        [else (cons (car lat) ((multirember-fa a test?) (cdr lat)))]))))

(module+ test
  (check-equal? ((multirember-fa 'tuna eq?) '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)))

; (define eq?-c ; from before (up the page)
;   (lambda (a)
;     (lambda (x)
;       (eq? x a))))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
      (cond
        [(null? lat) lat]
        [(test? (car lat)) (multiremberT test? (cdr lat))]
        [else (cons (car lat) (multiremberT test? (cdr lat)))])))

(module+ test
  (check-equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat) (col '() '())]
      [(eq? a (car lat)) (multirember&co a 
                                         (cdr lat) 
                                         (lambda (newlat seen) (col newlat (cons (car lat) seen))))]
      [else (multirember&co a (cdr lat)  (lambda (newlat seen) (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(module+ test [check-false (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)])
(module+ test [check-true  (multirember&co 'tuna '() a-friend)]) ; because lat is null => a-friend of '() '() will return #t

(module+ test [check-false (multirember&co 'tuna '(tuna) a-friend)])
; a == (car lat) => recurse on a:'tuna lat:'() and a new function (col x y)
; the next iteration => lat is null? => (col x y) where x:'() and y: (cons tuna '())=='(tuna) 
; because col is (a-friend above => y is not null => returns #f !!
; newlat and seen are arguments, and col and lat are predefined (fixed arguments)

;(define new-friend
;  (lambda (newlat seen)
;    (col newlat (cons 'tuna seen)))) ; col is predefined outside, lat too

;(define latest-friend
;  (lambda (newlat seen) 
;    (col (cons 'tuna newlat) seen))) ; col is predefined outside, lat too

;(multirember&co 'tuna '(tuna) a-friend)
; a == (car lat) => recurse on 'tuna '() and new-friend
; the next iteration => lat is null? => (new-friend on '() '() => #f

; (multirember&co 'tuna '(and tuna) a-friend)
; enter the else => recurse on 'tuna '(tuna) and latest-friend
; enter the eq?  => recurse on 'tuna '() and new-friend (where col is defined as latest-friend!!!)
; the next iteration => lat is null? => (col on '() '() where col is new-friend (where col is latest-friend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat))))]
      [else (cons (car lat) (multiinsertL new old (cdr lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) lat]
      [(eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) lat]
      [(eq? oldL oldR) lat]
      [(eq? oldL (car lat))
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? oldR (car lat))
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
      [else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat) (col '() 0 0)]
      [(eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat) 
                      (lambda (newlat L R)
                        (col (cons new (cons oldL newlat)) (add1 L) R)))]
      [(eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR (cons new newlat)) L (add1 R))))]
      [else
       (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons (car lat) newlat) L R)))])))

;(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define even?
  (lambda (n)
    (= (x (// n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
      [(null? l) l]
      [(atom? (car l)) 
              (cond
                [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
                [else (evens-only* (cdr l))])]
      [else (cons (evens-only* (car l)) (evens-only* (cdr l)))])))

(module+ test
  (check-equal? (evens-only* '(1 2 3 4 5)) '(2 4))
  (check-equal? (evens-only* '((1 2) (3 4) 5)) '((2) (4))))

(define evens-only*&co
  (lambda (l col)
    (cond
      [(null? l) (col l 1 0)]
      [(atom? (car l)) 
       (cond
         [(even? (car l)) 
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                          (col (cons (car l) newl)
                               (x (car l) p)
                               s)))]
         [else (evens-only*&co (cdr l)
                               (lambda (newl p s)
                                 (col newl
                                      p
                                      (+ (car l) s))))])]
      [else (evens-only*&co (car l) 
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl) (x ap dp) (+ as ds))))))])))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(module+ test
  (check-equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend) '(38 1920 (2 8) 10 (() 6) 2)))

