#lang racket
(require rackunit)

;; Stolen from sentientmonkey
(define Y
  (lambda (le)
    ((lambda (f) (f f))
       (lambda (f)
         (le (lambda (x) ((f f) x)))))))

;; Chapter 3 definition
;; (define multirember 
;;   (lambda (a lat)
;;     (cond
;;      [(null? lat) '()]
;;      [(eq? (car lat) a) (multirember a (cdr lat))]
;;      [else (cons (car lat) (multirember a (cdr lat)))])))

;; Y combinator definition
;; (define multirember
;;   (lambda (a lat)
;;     ((Y (lambda (mr)
;;           (lambda (lat)
;;             (cond
;;              [(null? lat) '()]
;;              [(eq? a (car lat))
;;               (mr (cdr lat))]
;;              [else (cons (car lat)
;;                          (mr (cdr lat)))]))))
;;      lat)))

;; letrec definition
;; (define multirember
;;   (lambda (a lat)
;;     ((letrec 
;;          ((mr (lambda (lat)
;;                 (cond 
;;                  [(null? lat) '()]
;;                  [(eq? a (car lat)) (mr (cdr lat))]
;;                  [else (cons (car lat) (mr (cdr lat)))]))))
;;        mr)
;;      lat)))

;; Definiton without Lambda 
;; (define multirember
;;   (letrec
;;       ((mr 
;;         (lambda (a lat)
;;           (cond
;;            [(null? lat) lat]
;;            [(eq? a (car lat)) (mr a (cdr lat))]
;;            [else (cons (car lat) (mr a (cdr lat)))]))))
;;     mr))

;; Rename
(define multirember
  (letrec
   ((multirember
     (lambda (a lat)
       (cond
        [(null? lat) lat]
        [(eq? (car lat) a) (multirember a (cdr lat))]
        [else (cons (car lat) (multirember a (cdr lat)))]))))
   multirember))

(test-case "multirember "
           [check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)]
           [check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte)])


;; (define length
;;   ((lambda (le)
;;      ((lambda (f) (f f))
;;       (lambda (f)
;;         (le (lambda (x) ((f f) x))))))
;;    (lambda (length)
;;      (lambda (l)
;;        (cond
;;         [(null? l) 0]
;;         [else (add1 (length (cdr l)))])))))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))))

(test-case "length"
           [check-eq? (length '()) 0]
           [check-eq? (length '(a)) 1]
           [check-eq? (length '(a b c)) 3])

;; From chapter 8
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond 
       [(null? l) l]
       [(test? a (car l)) (cdr l)]
       [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(define rember-eq? (rember-f eq?))

(test-case "rember-eq?"
           [check-equal? (rember-eq? 'tuna '(tuna salad is good)) '(salad is good)])

(test-case "rember-f"
           [check-equal? ((rember-f eq?) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad)]
           [check-equal? ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?)])


;; Definition 1
;; (define multirember-f
;;   (lambda (test?)
;;     (lambda (a l)
;;       (cond
;;        [(null? l) l]
;;        [(test? a (car l)) ((multirember-f test?) a (cdr l))]
;;        [else (cons (car l) ((multirember-f test?) a (cdr l)))]))))


(define multirember-f
  (lambda (test?)
    (letrec 
        ((m-f
          (lambda (a lat)
            (cond
             [(null? lat) lat]
             [(test? a (car lat)) (m-f a (cdr lat))]
             [else (cons (car lat) (m-f a (cdr lat)))]))))
      m-f)))

(define multirember-eq? (multirember-f eq?))

(test-case "multirember-eq?"
           [check-equal? (multirember-eq? 'tuna '(tuna salad is good tuna)) '(salad is good)])

(test-case "multirember-f"
           [check-equal? ((multirember-f eq?) 'salad '(shrimp salad and tuna salad)) '(shrimp and tuna)]
           [check-equal? ((multirember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair? eq?)) '(equal? eqan? eqlist? eqpair?)])


;; (define member?
;;   (lambda (a lat)
;;     (cond
;;      [(null? lat) #f]
;;      [(eq? (car lat) a) #t]
;;      [else (member? a (cdr lat))])))

;; Letrec definition
(define member?
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond
                   [(null? l) #f]
                   [(eq? a (car l)) #t]
                   [else (yes? (cdr l))]))))
       yes?)
     lat)))

(test-case "member?"
           [check-false (member? 'ice '(salad greens with pears brie cheese frozen yogurt))])


;; Simple definition
;; (define union
;;   (lambda (s1 s2)
;;     (cond 
;;      [(null? s1) s2]
;;      [(member? (car s1) s2) (union (cdr s1) s2)]
;;      [else (cons (car s1) (union (cdr s1) s2))])))
;; (define union
;;   (lambda (s1 s2)
;;     (letrec
;;         ((U (lambda (s)
;;               (cond
;;                [(null? s) s2]
;;                [(member? (car s) s2) (U (cdr s))]
;;                [else (cons (car s) (U (cdr s)))]))))
;;       (U s1))))
;; (define union
;;   (lambda (s1 s2)
;;     (letrec
;;         ((union (lambda (s)
;;               (cond
;;                [(null? s) s2]
;;                [(member? (car s) s2) (union (cdr s))]
;;                [else (cons (car s) (union (cdr s)))]))))
;;       (union s1))))
;; (define union
;;   (lambda (s1 s2)
;;     (letrec
;;         ((U (lambda (s)
;;               (cond
;;                [(null? s) s2]
;;                [(member? (car s) s2) (U (cdr s))]
;;                [else (cons (car s) (U (cdr s)))])))
;;          (member? (lambda (a lat)
;;                     (cond
;;                      [(null? lat) #f]
;;                      [(eq? (car lat) a) #t]
;;                      [else (member? a (cdr lat))]))))
;;       (U s1))))
(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (s)
              (cond
               [(null? s) s2]
               [(M? (car s) s2) (U (cdr s))]
               [else (cons (car s) (U (cdr s)))])))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                           [(null? lat) #f]
                           [(eq? (car lat) a) #t]
                           [else (N? (cdr lat))]))))
                 (N? lat)))))
      (U s1))))

(test-case "union"
           [check-equal? (union '(tomatoes and macaroni casserole) '(macaroni and cheese)) '(tomatoes casserole macaroni and cheese)])


;; (define two-in-a-row?
;;   (lambda (lat)
;;     (letrec
;;         ((W (lambda (p lat)
;;               (cond
;;                [(null? lat) #f]
;;                [else (or (eq? (car lat) p)
;;                          (W (car lat) (cdr lat)))]))))
;;       (cond
;;        ((null? lat) #f)
;;        (else (W (car lat) (cdr lat)))))))

(define two-in-a-row?
  (letrec 
      ((W (lambda (a lat)
            (cond
             [(null? lat) #f]
             [else (or (eq? (car lat) a)
                       (W (car lat) (cdr lat)))]))))
    (lambda (lat)
      (cond 
       [(null? lat) #f]
       [else (W (car lat) (cdr lat))]))))


(test-case "two-in-a-row?"
           [check-false (two-in-a-row? '(Italian sardines spaghetti parsley))]
           [check-true (two-in-a-row? '(Italian sardines sardines spaghetti parsley))]
           [check-false (two-in-a-row? '(Italian sardines more sardines spaghetti))]
           [check-true (two-in-a-row? '(b d e i i a g))])


;; (define sum-of-prefixes-b
;;   (lambda (sonssf tup)
;;     (cond
;;      [(null? tup) '()]
;;      [else (cons (+ sonssf (car tup))
;;                  (sum-of-prefixes-b 
;;                   (+ sonssf (car tup))
;;                   (cdr tup)))])))
;; (define sum-of-prefixes
;;   (lambda (tup)
;;     (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes
  (letrec
      ((H (lambda (sss tup)
            (cond
             [(null? tup) '()]
             [else (cons (+ sss (car tup))
                         (H (+ sss (car tup)) (cdr tup)))]))))
    (lambda (tup)
      (H 0 tup))))

(test-case "sum-of-prefixes"
           [check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29)]
           [check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5)]
           [check-equal? (sum-of-prefixes '()) '()])


(define scramble 
  (lambda (tup)
    (letrec
        [(pick (lambda (n lat)
                 (cond
                  [(eq? n 1) (car lat)]
                  [else (pick (sub1 n) (cdr lat))])))
         (P (lambda (tup rev-pre)
              (cond
               [(null? tup) '()]
               [else 
                (cons (pick (car tup)
                            (cons (car tup) rev-pre))
                      (P (cdr tup)
                         (cons (car tup) rev-pre)))])))]
      (P tup '()))))

(test-case "scramble"
           [check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9)]
           [check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1)]
           [check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)])
