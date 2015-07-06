#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(provide -o +o poso full-addero addero >1o)

;; 5
;; I like building up from nand :]
(define (bit-nando x y r)
  (conde
    [(== 0 x) (== 0 y) (== 1 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]
    [else u#]))

(define (bit-xoro x y r)
  (fresh (s t u)
    (bit-nando x y s)
    (bit-nando x s t)
    (bit-nando s y u)
    (bit-nando t u r)))

;; 6
(check-run* (s)
            (fresh (x y)
              (bit-xoro x y 0)
              (== `(,x ,y) s))
            => '((0 0)
                 (1 1)))

;; 8
(check-run* (s)
            (fresh (x y)
              (bit-xoro x y 1)
              (== `(,x ,y) s))
            => '((1 0)
                 (0 1)))

;; 9
(check-run* (s)
            (fresh (x y r)
              (bit-xoro x y r)
              (== `(,x ,y ,r) s))
            => '((0 0 0)
                 (1 0 1)
                 (0 1 1)
                 (1 1 0)))

;; 10
(define (bit-noto x r)
  (bit-nando x x r))

(define (bit-ando x y r)
  (fresh (s)
    (bit-nando x y s)
    (bit-noto s r)))

;; 11
(check-run* (s)
            (fresh (x y r)
              (bit-ando x y 1)
              (== `(,x ,y) s))
            => '((1 1)))

;; 12
(define (half-addero x y r c)
  (all
    (bit-xoro x y r)
    (bit-ando x y c)))

(check-run* (r)
            (half-addero 1 1 r 1)
            => '(0))

;; 13
(check-run* (s)
            (fresh (x y r c)
              (half-addero x y r c)
              (== `(,x ,y ,r ,c) s))
            => '((0 0 0 0)
                 (1 0 1 0)
                 (0 1 1 0)
                 (1 1 0 1)))

;; 14
;; it's a half-bit adder that adds x to y with a
;; carryover bit c and a carryout bit r

;; 15
(define (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(check-run* (s)
            (fresh (r c)
              (full-addero 0 1 1 r c)
              (== `(,r ,c) s))
            => '((0 1)))

;; 16
(check-run* (s)
            (fresh (r c)
              (full-addero 1 1 1 r c)
              (== `(,r ,c) s))
            => '((1 1)))

;; 17
(check-run* (s)
            (fresh (b x y r c)
              (full-addero b x y r c)
              (== `(,b ,x ,y ,r ,c) s))
            => '((0 0 0 0 0)
                 (1 0 0 1 0)
                 (0 1 0 1 0)
                 (1 1 0 0 1)
                 (0 0 1 1 0)
                 (1 0 1 0 1)
                 (0 1 1 0 1)
                 (1 1 1 1 1)))
;; 18
;; b + x + y = r with a carry out bit of c

;; 43
(define (build-num n)
  (cond
    [(zero? n) '()]
    [(even? n) (cons 0 (build-num (/ n 2)))]
    [else (cons 1 (build-num (/ (sub1 n) 2)))]))

;; 31
(check-equal? (build-num 19) '(1 1 0 0 1))

;; 32
(check-equal? (build-num 17290) '(0 1 0 1 0 0 0 1 1 1 0 0 0 0 1))

;; 40
(check-equal? (build-num 0) '())

;; 41
(check-equal? (build-num 36) '(0 0 1 0 0 1))

;; 42
(check-equal? (build-num 19) '(1 1 0 0 1))

;; 80
(define (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

;; 81
(check-run* (q)
            (poso '(0 1 1))
            (== #t q)
            => '(#t))

;; 82
(check-run* (q)
            (poso '(1))
            (== #t q)
            => '(#t))

;; 82
(check-run* (q)
            (poso '())
            (== #t q)
            => '())
;; 83
(check-run* (r)
            (poso r)
            => '((_.0 . _.1)))

;; 86
(define (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(check-run* (q)
            (>1o '(0 1 1))
            (== #t q)
            => '(#t))

;; 87
(check-run* (q)
            (>1o '(0 1))
            (== #t q)
            => '(#t))

;; 88
(check-run* (q)
            (>1o '(1))
            (== #t q)
            => '())

;; 89
(check-run* (q)
            (>1o '())
            (== #t q)
            => '())

;; 90
(check-run* (r)
            (>1o r)
            => '((_.0 _.1 . _.2)))

;; 118
(define (addero d n m r)
  (condi
    [(== 0 d) (== '() m) (== n r)]
    [(== 0 d) (== '() n) (== m r) (poso m)]
    [(== 1 d) (== '() m) (addero 0 n '(1) r)]
    [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
    [(== '(1) n) (== '(1) m) (fresh (a c)
                               (== `(,a ,c) r)
                               (full-addero d 1 1 a c))]
    [(== '(1) n) (gen-addero d n m r)]
    [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
    [(>1o n) (gen-addero d n m r)]
    [else u#]))

(define (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (alli
      (full-addero d a b c e)
      (addero e x y z))))

;; 96
(check-run 3 (s)
           (fresh (x y r)
             (addero 0 x y r)
             (== `(,x ,y ,r) s))
           => '((_.0 () _.0)
                (() (_.0 . _.1) (_.0 . _.1))
                ((1) (1) (0 1))))

;; 101
(check-run 22 (s)
           (fresh (x y r)
             (addero 0 x y r)
             (== `(,x ,y ,r) s))
           => '((_.0 () _.0)
                (() (_.0 . _.1) (_.0 . _.1))
                ((1) (1) (0 1))
                ((1) (0 _.0 . _.1) (1 _.0 . _.1))
                ((0 _.0 . _.1) (1) (1 _.0 . _.1))
                ((1) (1 1) (0 0 1))
                ((0 1) (0 1) (0 0 1))
                ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))
                ((1 1) (1) (0 0 1))
                ((1) (1 1 1) (0 0 0 1))
                ((1 1) (0 1) (1 0 1))
                ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
                ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
                ((1) (1 1 1 1) (0 0 0 0 1))
                ((0 1) (0 0 _.0 . _.1) (0 1 _.0 . _.1))
                ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
                ((1 1 1) (1) (0 0 0 1))
                ((1) (1 1 1 1 1) (0 0 0 0 0 1))
                ((0 1) (1 1) (1 0 1))
                ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
                ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
                ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))))

;; 104
(define (width n)
  (cond
    [(null? n) 0]
    [(pair? n) (+ (width (cdr n)) 1)]
    [else 1]))

(define (all? fn l)
  (cond
    [(null? l) #t]
    [else (and (fn (car l))
               (all? fn (cdr l)))]))

(check-true
  (all? (lambda (n) (eq? (width n) 3))
        (run 22 (s)
          (fresh (x y r)
            (addero 0 x y r)
            (== `(,x ,y ,r) s)))))

;; 120
(check-run* (s)
            (gen-addero 1 '(0 1 1) '(1 1) s)
            => '((0 1 0 1)))

;; 126
(check-run* (s)
            (fresh (x y)
              (addero 0 x y '(1 0 1))
              (== `(,x ,y) s))
            => '(((1 0 1) ())
                 (() (1 0 1))
                 ((1) (0 0 1))
                 ((0 0 1) (1))
                 ((1 1) (0 1))
                 ((0 1) (1 1))))

;; 127
;; all of the numbers that add up to (1 0 1) aka 5

;; 128
(define (+o n m k)
  (addero 0 n m k))

(check-run* (s)
            (fresh (x y)
              (+o x y '(1 0 1))
              (== `(,x ,y) s))
            => '(((1 0 1) ())
                 (() (1 0 1))
                 ((1) (0 0 1))
                 ((0 0 1) (1))
                 ((1 1) (0 1))
                 ((0 1) (1 1))))

;; 130
(define (-o n m k)
  (+o m k n))

;; 131
(check-run* (q)
            (-o '(0 0 0 1) '(1 0 1) q)
            => '((1 1)))

;; 132
(check-run* (q)
            (-o '(0 1 1) '(0 1 1) q)
            => '(()))

;; 133
(check-run* (q)
            (-o '(0 1 1) '(0 0 0 1) q)
            => '())

