#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch04.rkt")
(require "ch06.rkt")

(define remember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) (quote ())]
        [(test? (car l) a) (cdr l)]
        [else
          (cons (car l)
                ((remember-f test?) a (cdr l)))]))))

(check-equal? ((remember-f =) 5 '(6 2 5 3)) '(6 2 3))
(check-equal? ((remember-f eq?) 'jelly '(jelly beans are good)) '(beans are good))
(check-equal? ((remember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(check-true ((eq?-c 'salad) 'salad))
(check-false ((eq?-c 'salad) 'tuna))

(define eq?-salad
  (eq?-c 'salad))

(check-true (eq?-salad 'salad))
(check-false (eq?-salad 'tuna))

(check-equal? ((remember-f eq?) 'tuna '(shrimp salad and tuna salad))
              '(shrimp salad and salad))

(check-equal? ((remember-f eq?) 'eq? '(equal? eq? eqan? eqlist? epair?))
              '(equal? eqan? eqlist? epair?))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) (quote ())]
        [(test? (car lat) old)
         (cons new lat)]
        [else (cons (car lat)
                    ((insertL-f test?) new old
                                       (cdr lat)))]))))

(check-equal? ((insertL-f eq?) 'fudge 'topping '(ice cream with topping for desert))
              '(ice cream with fudge topping for desert))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) (quote ())]
        [(test? (car lat) old)
         (cons old
               (cons new (cdr lat)))]
        [else (cons (car lat)
                    ((insertR-f test?) new old
                                       (cdr lat)))]))))

(check-equal? ((insertR-f eq?) 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with fudge topping for desert))

(define seqL
  (lambda (new old lat)
    (cons new
          (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old
          (cons new lat))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        [(null? lat) (quote ())]
        [(eq? (car lat) old)
         (seq new old (cdr lat))]
        [else (cons (car lat)
                    ((insert-g seq) new old
                                    (cdr lat)))]))))

(check-equal? ((insert-g seqL) 'fudge 'topping '(ice cream with topping for desert))
              '(ice cream with fudge topping for desert))

(check-equal? ((insert-g seqR) 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with fudge topping for desert))

(define insertR
  (insert-g seqR))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with fudge topping for desert))

(define insertL
  (insert-g seqL))

(check-equal? (insertL 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with topping fudge for desert))

(define insertL2
  (insert-g
    (lambda (new old lat)
      (cons new
            (cons old lat)))))

(check-equal? (insertL2 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with topping fudge for desert))

(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst (insert-g seqS))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for desert))
              '(ice cream with topping for desert))

(define seqrem
  (lambda (new old l) l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(check-equal? (rember 'sausage '(pizza with sausage and bacon))
              '(pizza with and bacon))


(define atom-to-function
  (lambda (a)
    (cond
      [(eq? a '+) o+]
      [(eq? a 'x) x]
      [else pow])))

(check-equal? (atom-to-function (operator '(+ 5 3))) o+)

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))])))

(check-equal? (value '(+ 3 4)) 7)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) (quote ())]
        [(test? (car lat) a)
         ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat)
                    ((multirember-f test?) a (cdr lat)))]))))

(check-equal? ((multirember-f eq?) 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember-eq
  (multirember-f eq?))

(check-equal? (multirember-eq 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define eq?-tuna
  (eq?-c 'tuna))

(check-true (eq?-tuna 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      [(null? lat) (quote ())]
      [(test? (car lat))
       (multiremberT test? (cdr lat))]
      [else (cons (car lat)
                  (multiremberT test? (cdr lat)))])))

(check-equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat)
       (col (quote ()) (quote ()))]
      [(eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen))))]
      [else
        (multirember&co a (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(check-true (multirember&co 'tuna '() a-friend))
(check-false (multirember&co 'tuna '(tuna) a-friend))
(check-false (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons 'tuna seen))))

(check-false (multirember&co 'tuna '(and tuna) a-friend))

(define last-friend
  (lambda (x y)
    (length x)))

(check-equal? (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend) 3)


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old
                                (cdr lat)))))))

(check-equal? (multiinsertR 'vanilla 'chocolate '(chocolate ice cream sundae with chocolate sauce))
              '(chocolate vanilla ice cream sundae with chocolate vanilla sauce))

; multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old
                                (cdr lat)))))))

(check-equal? (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
              '(chips and fried fish or fried fish and fried))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) (quote ())]
      [(eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR
                                  (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR
                                  (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))])))


(check-equal? (multiinsertLR 'fried 'fish 'chocolate '(chips and fish or fish and chocolate fried))
              '(chips and fried fish or fried fish and chocolate fried fried))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat) (col (quote ()) 0 0)]
      [(eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R)))]
      [(eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R))))]
      [else
        (multiinsertLR&co new oldL oldR
                       (cdr lat)
                       (lambda (newlat L R)
                         (col (cons (car lat) newlat) L R)))])))

(check-equal? (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
                (lambda (newlat L R)
                    (cons L (cons R newlat))))
              '(2 2 chips salty and salty fish or salty fish and chips salty))

(define even?
  (lambda (n)
    (eq (x (div n 2) 2) n)))

(check-false (even? 5))
(check-true (even? 6))

(define evens-only*
  (lambda (l)
    (cond
      [(null? l) (quote ())]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (cons (car l) (evens-only* (cdr l)))]
         [else
           (evens-only* (cdr l))])]
       [else
         (cons (evens-only* (car l))
               (evens-only* (cdr l)))])))

(check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6 2)))
              '((2 8) 10 (() 6 2)))

(define evens-only*&co
  (lambda (l col)
    (cond
      [(null? l)
       (col (quote ()) 1 0)]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (x (car l) p) s)))]
         [else
           (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl
                                  p (o+ (car l) s))))])]
       [else
         (evens-only*&co (car l)
                         (lambda (al ap as)
                           (evens-only*&co (cdr l)
                             (lambda (dl dp ds)
                               (col (cons al dl)
                                    (x ap dp)
                                    (o+ as ds))))))])))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(check-equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
              '(38 1920 (2 8) 10 (() 6) 2))
