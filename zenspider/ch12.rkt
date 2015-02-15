#lang racket/base

(require "lib/shared.rkt")
(require "ch09.rkt")

(module+ test
  (require rackunit))

(define multirember1                    ; pg 17
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             [(null? lat) '()]
             [(eq? a (car lat)) (mr (cdr lat))]
             [else (cons (car lat)
                         (mr (cdr lat)))]))))
     lat)))

(module+ test
  (define (test/multirember multirember)
    (check-equal? (multirember 'b '(b a b c b d b e b)) ; taken from ch03
                  '(a c d e))))

(module+ test
  (test/multirember multirember1))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))))

(module+ test
  (check-equal? (length '(1 2 3))
                3))

(define multirember2                    ; pg 22
  (lambda (a lat)
    (letrec ([mr (lambda (lat)
                   (cond [(null? lat) '()]
                         [(eq? a (car lat)) (mr (cdr lat))]
                         [else (cons (car lat)
                                     (mr (cdr lat)))]))])
      (mr lat))))

(module+ test
  (test/multirember multirember2))

(define multirember-f                   ; pg 23
  (lambda (test?)
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a) ((multirember-f test?) a (cdr lat))]
            [else (cons (car lat)
                        ((multirember-f test?) a (cdr lat)))]))))

(module+ test
  (test/multirember (multirember-f eq?)))

(define multirember-f2
  (lambda (test?)
    (letrec ([m-f (lambda (a lat)
                    (cond
                      [(null? lat) '()]
                      [(test? (car lat) a)  (m-f a (cdr lat))]
                      [else (cons (car lat)
                                  (m-f a (cdr lat)))]))])
      m-f)))

(module+ test
  (test/multirember (multirember-f2 eq?)))

(define member1?                        ; pg 26
  (lambda (a lat)
    ((letrec ([yes? (lambda (l)
                      (cond [(null? l) #f]
                            [(eq? (car l) a) #t]
                            [else (yes? (cdr l))]))])
       yes?)
     lat)))

(module+ test
  (define (test/member? member?)
    (check-true  (member? 'b '(a b c)))
    (check-false (member? 'd '(a b c)))))

(module+ test
  (test/member? member1?))

(define member2?                        ; pg 27
  (lambda (a lat)
    (letrec ([yes? (lambda (l)
                     (cond [(null? l) #f]
                           [(eq? (car l) a) #t]
                           [else (yes? (cdr l))]))])
      (yes? lat))))

(module+ test
  (test/member? member2?))

(define union1                          ; pg 27
  (lambda (set1 set2)
    (cond [(null? set1) set2]
          [(member2? (car set1) set2) (union1 (cdr set1) set2)]
          [else (cons (car set1)
                      (union1 (cdr set1) set2))])))

(module+ test
  (define (test/union union)
    (check-equal? (union '(stewed tomatoes and macaroni casserole) ; from ch07
                         '(macaroni and cheese))
                  '(stewed tomatoes casserole macaroni and cheese))))

(module+ test
  (test/union union1))

(define union2                          ; pg 28
  (lambda (set1 set2)
    (letrec ([U (lambda (set)
                  (cond [(null? set) set2]
                        [(member2? (car set) set2) (U (cdr set))]
                        [else (cons (car set)
                                    (U (cdr set)))]))])
      (U set1))))

(module+ test
  (test/union union2))

(define union3
  (lambda (set1 set2)
    (letrec ([U (lambda (set)
                  (cond [(null? set) set2]
                        [(M? (car set) set2)  (U (cdr set))]
                        [else (cons (car set)
                                    (U (cdr set)))]))]
             [M? (lambda (a lat)
                   (letrec ([N? (lambda (lat)
                                  (cond [(null? lat) #f]
                                        [(eq? (car lat) a) #t]
                                        [else (N? (cdr lat))]))])
                     (N? lat)))])
      (U set1))))

(module+ test
  (test/union union3))

(define two-in-a-row?
  (letrec ([W (lambda (a lat)
                (cond [(null? lat) #f]
                      [else (or (eq? (car lat) a)
                                (W (car lat) (cdr lat)))]))])
    (lambda (lat)
      (cond [(null? lat) #f]
            [else (W (car lat) (cdr lat))]))))

(module+ test
  (test-case "two-in-a-row?"
    (check-false (two-in-a-row? '()))   ; from ch11
    (check-false (two-in-a-row? '(a b c d)))
    (check-true (two-in-a-row? '(a b b d)))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec ([S (lambda (sss tup)
                  (cond [(null? tup) '()]
                        [else (cons (+ sss (car tup))
                                    (S (+ sss (car tup)) (cdr tup)))]))])
      (S 0 tup))))

(module+ test
  (test-case "sum-of-prefixes"
    (check-equal? (sum-of-prefixes '(1 1 1 1 1))
                  '(1 2 3 4 5))
    (check-equal? (sum-of-prefixes '(2 1 9 17 0))
                  '(2 3 12 29 29))))
