#lang racket/base

(provide (all-defined-out))

(require "lib/reasonable.rkt")
(require "ch22.rkt")
(require "ch23.rkt")

(define (queens1 b)
  (fresh (q1)
    (queens b
            (list q1)
            (nums '(1)))))

(define (queens4 b)
  (fresh (q1 q2 q3 q4)
    (queens b
            (list q1 q2 q3 q4)
            (nums '(1 2 3 4)))))

(define (queens6 b)
  (fresh (q1 q2 q3 q4 q5 q6)
    (queens b
            (list q1 q2 q3 q4 q5 q6)
            (nums '(1 2 3 4 5 6)))))

(define (queens8 b)
  (fresh (q1 q2 q3 q4 q5 q6 q7 q8)
    (queens b
            (list q1 q2 q3 q4 q5 q6 q7 q8)
            (nums '(1 2 3 4 5 6 7 8)))))

(define (queens b vars vals)
  (all
   (≈ b vars)

   (domain b vals)

   (distinct° b)

   (safe b)))

(define (domain l dom)                  ; aka members°
  (cond-e [(null° l)]
          [(fresh (a d)
             (cons° a d l)
             (member° a dom)
             (domain d dom))]))

(define (safe b)
  (cond-a [(null° b) %s]
          [else (fresh (a d)
                  (cons° a d b)
                  (none-equal-up° a d)
                  (none-equal-down° a d)
                  (safe d))]))

(define (distinct° l)
  (cond-e [(null° l)]
          [(fresh (a d)
             (cons° a d l)
             (cond-a [(member° a d) %u]
                     [else (distinct° d)]))]))

(define (sub1° n m)
  (cond-a [(null° n) (≈ n m)]
          [(fresh (_)
             (cons° m _ n))]))

(define (add1° n m)
  (cons° n '() m))

(define (none-equal-up° n l)
  (cond-a [(null° l) %s]
          [else (fresh (a d n+1)
                  (add1° n n+1)
                  (cons° a d l)
                  (cond-a [(≈ n+1 a) %u]
                          [else (none-equal-up° n+1 d)]))]))

(define (none-equal-down° n l)
  (cond-a [(null° l) %s]
          [else (fresh (a d n-1)
                  (sub1° n n-1)
                  (cons° a d l)
                  (cond-a [(≈ n-1 a) %u]
                          [else (none-equal-down° n-1 d)]))]))

(define (-° n m d)
  (cond-a [(null° n) (≈ n d)]           ; no negatives
          [(null° m) (≈ n d)]
          [else (fresh (na nd ma md)
                  (cons° na nd n)
                  (cons° ma md m)
                  (-° na ma d))]))      ; recurse on cars

(define (n->l n)
  (cond [(zero? n) '()]
        [else (cons (n->l (sub1 n)) '())]))

(define (l->n l)
  (cond [(null? l) 0]
        [else (add1 (l->n (car l)))]))

(define (nums l) (map n->l l))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-equal? (n->l 0) '())
  (check-equal? (n->l 1) '(()))
  (check-equal? (n->l 2) '((())))
  (check-equal? (n->l 3) '(((()))))

  (check-equal? (l->n '())           0)
  (check-equal? (l->n '(()))         1)
  (check-equal? (l->n '((())))       2)
  (check-equal? (l->n '(((()))))     3)

  (check-run* (x) (-° (n->l 3) (n->l 3) x) => (list (n->l 0)))
  (check-run* (x) (-° (n->l 3) (n->l 2) x) => (list (n->l 1)))
  (check-run* (x) (-° (n->l 3) (n->l 1) x) => (list (n->l 2)))
  (check-run* (x) (-° (n->l 3) (n->l 0) x) => (list (n->l 3)))

  (check-run* (x) (-° (n->l 1) (n->l 3) x) => (list (n->l 0))) ; no negatives

  (check-run* (x) (sub1° (n->l 0) x) => (list (n->l 0)))
  (check-run* (x) (sub1° (n->l 1) x) => (list (n->l 0)))
  (check-run* (x) (sub1° (n->l 2) x) => (list (n->l 1)))

  (check-run* (x) (add1° (n->l 0) x) => (list (n->l 1)))
  (check-run* (x) (add1° (n->l 1) x) => (list (n->l 2)))
  (check-run* (x) (add1° (n->l 2) x) => (list (n->l 3)))

  (define-check (check-none-equal-up° nn ll exp)
    (check-run* (x)
                (fresh (n l)
                  (≈ n (n->l nn))
                  (≈ l (nums ll))
                  (none-equal-up° n l)
                  (≈ x #t))
                => exp))

  (check-none-equal-up° 1 '()      good)
  (check-none-equal-up° 1 '(1)     good)
  (check-none-equal-up° 1 '(3)     good)
  (check-none-equal-up° 1 '(3 4 5) good)
  (check-none-equal-up° 1 '(2)     none)
  (check-none-equal-up° 1 '(4 3 1) none)

  (define-check (check-none-equal-down° nn ll exp)
    (check-run* (x)
                (fresh (n l)
                  (≈ n (n->l nn))
                  (≈ l (nums ll))
                  (none-equal-down° n l)
                  (≈ x #t))
                => exp))

  (check-none-equal-down° 3 '()      good)
  (check-none-equal-down° 3 '(3)     good)
  (check-none-equal-down° 3 '(1)     good)
  (check-none-equal-down° 3 '(1 2 3) good)
  (check-none-equal-down° 3 '(2)     none)
  (check-none-equal-down° 3 '(4 3 0) none)

  (check-none-equal-down° 3 '(1 4 2) good)
  (check-none-equal-up° 3 '(1 4 2) good)

  (define-check (check-safe l exp)
    (check-run* (x)
                (fresh (b)
                  (≈ b (nums l))
                  (safe b)
                  (≈ x #t))
                => exp))

  (check-safe '(1)       good)
  (check-safe '(1 1)     good)          ; doesn't check distinctness
  (check-safe '(1 2)     none)          ; up
  (check-safe '(2 1)     none)          ; down
  (check-safe '(3 1)     good)
  (check-safe '(3 1 4 2) good)

  (define (num-list l)
    (map nums l))

  (time                          ; cpu time: 0 real time: 1 gc time: 0
   (check-run* (b) (queens1 b) => (num-list '((1)))))

  (time                       ; cpu time: 39 real time: 39 gc time: 35
   (check-run* (b) (queens4 b) => (num-list '((2 4 1 3)
                                              (3 1 4 2)))))

  (time                   ; cpu time: 1260 real time: 1263 gc time: 12
   (check-run* (b) (queens6 b) => (num-list '((2 4 6 1 3 5)
                                              (3 6 2 5 1 4)
                                              (4 1 5 2 6 3)
                                              (5 3 1 6 4 2)))))

  (time             ; cpu time: 704202 real time: 711508 gc time: 6506
   (check-run* (b) (queens8 b) => (num-list '((1 5 8 6 3 7 2 4)
                                              (1 6 8 3 7 4 2 5)
                                              (1 7 4 6 8 2 5 3)
                                              (1 7 5 8 2 4 6 3)
                                              (2 4 6 8 3 1 7 5)
                                              (2 5 7 1 3 8 6 4)
                                              (2 5 7 4 1 8 6 3)
                                              (2 6 1 7 4 8 3 5)
                                              (2 6 8 3 1 4 7 5)
                                              (2 7 3 6 8 5 1 4)
                                              (2 7 5 8 1 4 6 3)
                                              (2 8 6 1 3 5 7 4)
                                              (3 1 7 5 8 2 4 6)
                                              (3 5 2 8 1 7 4 6)
                                              (3 5 2 8 6 4 7 1)
                                              (3 5 7 1 4 2 8 6)
                                              (3 5 8 4 1 7 2 6)
                                              (3 6 2 5 8 1 7 4)
                                              (3 6 2 7 1 4 8 5)
                                              (3 6 2 7 5 1 8 4)
                                              (3 6 4 1 8 5 7 2)
                                              (3 6 4 2 8 5 7 1)
                                              (3 6 8 1 4 7 5 2)
                                              (3 6 8 1 5 7 2 4)
                                              (3 6 8 2 4 1 7 5)
                                              (3 7 2 8 5 1 4 6)
                                              (3 7 2 8 6 4 1 5)
                                              (3 8 4 7 1 6 2 5)
                                              (4 1 5 8 2 7 3 6)
                                              (4 1 5 8 6 3 7 2)
                                              (4 2 5 8 6 1 3 7)
                                              (4 2 7 3 6 8 1 5)
                                              (4 2 7 3 6 8 5 1)
                                              (4 2 7 5 1 8 6 3)
                                              (4 2 8 5 7 1 3 6)
                                              (4 2 8 6 1 3 5 7)
                                              (4 6 1 5 2 8 3 7)
                                              (4 6 8 2 7 1 3 5)
                                              (4 6 8 3 1 7 5 2)
                                              (4 7 1 8 5 2 6 3)
                                              (4 7 3 8 2 5 1 6)
                                              (4 7 5 2 6 1 3 8)
                                              (4 7 5 3 1 6 8 2)
                                              (4 8 1 3 6 2 7 5)
                                              (4 8 1 5 7 2 6 3)
                                              (4 8 5 3 1 7 2 6)
                                              (5 1 4 6 8 2 7 3)
                                              (5 1 8 4 2 7 3 6)
                                              (5 1 8 6 3 7 2 4)
                                              (5 2 4 6 8 3 1 7)
                                              (5 2 4 7 3 8 6 1)
                                              (5 2 6 1 7 4 8 3)
                                              (5 2 8 1 4 7 3 6)
                                              (5 3 1 6 8 2 4 7)
                                              (5 3 1 7 2 8 6 4)
                                              (5 3 8 4 7 1 6 2)
                                              (5 7 1 3 8 6 4 2)
                                              (5 7 1 4 2 8 6 3)
                                              (5 7 2 4 8 1 3 6)
                                              (5 7 2 6 3 1 4 8)
                                              (5 7 2 6 3 1 8 4)
                                              (5 7 4 1 3 8 6 2)
                                              (5 8 4 1 3 6 2 7)
                                              (5 8 4 1 7 2 6 3)
                                              (6 1 5 2 8 3 7 4)
                                              (6 2 7 1 3 5 8 4)
                                              (6 2 7 1 4 8 5 3)
                                              (6 3 1 7 5 8 2 4)
                                              (6 3 1 8 4 2 7 5)
                                              (6 3 1 8 5 2 4 7)
                                              (6 3 5 7 1 4 2 8)
                                              (6 3 5 8 1 4 2 7)
                                              (6 3 7 2 4 8 1 5)
                                              (6 3 7 2 8 5 1 4)
                                              (6 3 7 4 1 8 2 5)
                                              (6 4 1 5 8 2 7 3)
                                              (6 4 2 8 5 7 1 3)
                                              (6 4 7 1 3 5 2 8)
                                              (6 4 7 1 8 2 5 3)
                                              (6 8 2 4 1 7 5 3)
                                              (7 1 3 8 6 4 2 5)
                                              (7 2 4 1 8 5 3 6)
                                              (7 2 6 3 1 4 8 5)
                                              (7 3 1 6 8 5 2 4)
                                              (7 3 8 2 5 1 6 4)
                                              (7 4 2 5 8 1 3 6)
                                              (7 4 2 8 6 1 3 5)
                                              (7 5 3 1 6 8 2 4)
                                              (8 2 4 1 7 5 3 6)
                                              (8 2 5 3 1 7 4 6)
                                              (8 3 1 6 2 5 7 4)
                                              (8 4 1 3 6 2 7 5)))))

  'done
  )

;; (define permute
;;   (lambda (l1 l2)
;;     (cond-e
;;       [(fresh (x)
;;          (cons° x '() l1)
;;          (cons° x '() l2))]
;;       [(fresh (x y z1)
;;          (cons° x y l1)
;;          (=/= y '())
;;          (permute y z1)
;;          (do-insert x z1 l2))])))
