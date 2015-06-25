#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(define bit-xoro
  (lambda (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 0 x) (== 1 y) (== 1 r)]
     [(== 1 x) (== 0 y) (== 1 r)]
     [(== 1 x) (== 1 y) (== 0 r)])))


;; 6
[check-equal? (run* (s)
                    (fresh (x y)
                           (bit-xoro x y 0)
                           (== `(,x ,y) s)))
              '((0 0) (1 1))]

;; 8
[check-equal? (run* (s)
                    (fresh (x y)
                           (bit-xoro x y 1)
                           (== `(,x ,y) s)))
              '((0 1) (1 0))]

;; 9
[check-equal? (run* (s)
                    (fresh (x y r)
                           (bit-xoro x y r)
                           (== `(,x ,y ,r) s)))
              '((0 0 0)
                (0 1 1)
                (1 0 1)
                (1 1 0))]

;; 10
(define bit-ando
  (lambda (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 0 x) (== 1 y) (== 0 r)]
     [(== 1 x) (== 0 y) (== 0 r)]
     [(== 1 x) (== 1 y) (== 1 r)])))

;; 11
[check-equal? (run* (s)
                    (fresh (x y)
                           (bit-ando x y 1)
                           (== `(,x ,y) s)))
              '((1 1))]

;; 12
(define half-addero
  (lambda (x y r c)
          (all
           (bit-xoro x y r)
           (bit-ando x y c))))

[check-equal? (run* (r)
                    (half-addero 1 1 r 1))
              '(0)]

;; 13
[check-equal? (run* (s)
                    (fresh (x y r c)
                           (half-addero x y r c)
                           (== `(,x ,y ,r ,c) s)))
              '((0 0 0 0)
                (0 1 1 0)
                (1 0 1 0)
                (1 1 0 1))] ;; YAY TRUTH TABLES!

;; 14
;; r is the binary digit sum of x & y and c is the carry

;; 15
(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
           (half-addero x y w xy)
           (half-addero w b r wz)
           (bit-xoro xy wz c))))

[check-equal? (run* (s)
                    (fresh (r c)
                           (full-addero 0 1 1 r c)
                           (== `(,r ,c) s)))
              '((0 1))]

;; 16
[check-equal? (run* (s)
                    (fresh (r c)
                           (full-addero 1 1 1 r c)
                           (== `(,r ,c) s)))
              '((1 1))]

;; 17
;; TO DISCUSS HOW THE HELL DID THIS HAPPEN!
[check-equal? (run* (s)
                    (fresh (b x y r c)
                           (full-addero b x y r c)
                           (== `(,b ,x ,y ,r ,c) s)))
              '((0 0 0 0 0)  ;;AEI
                (1 0 0 1 0)  ;;AFI
                (0 0 1 1 0)  ;;BGI
                (1 0 1 0 1)  ;;BHJ
                (0 1 0 1 0)  ;;CGI
                (1 1 0 0 1)  ;;CHJ
                (0 1 1 0 1)  ;;DEK
                (1 1 1 1 1)) ;;DFK
              ]


;; 18
;; Given two inputs and a carry in bit. The full adder calculates the
;; result and the carry out.


;; 43
(define build-num
  (lambda (n)
    (cond
     [(zero? n) '()]
     [(and (not (zero? n)) (even? n))
      (cons 0 (build-num (/ n 2)))]
     [(odd? n) (cons 1 (build-num (/ (- n 1) 2)))])))

;; 40
[check-equal? (build-num 0) '()]

;; 41
[check-equal? (build-num 36) '(0 0 1 0 0 1)]

;; 42
[check-equal? (build-num 19) '(1 1 0 0 1)]

;; 44
(define build-num2
  (lambda (n)
    (cond
     [(odd? n) (cons 1 (build-num (/ (- n 1) 2)))]
     [(and (not (zero? n)) (even? n))
      (cons 0 (build-num (/ n 2)))]
     [(zero? n) '()])))


(test-case "build-num2"
           [check-equal? (build-num2 0) '()]
           [check-equal? (build-num2 36) '(0 0 1 0 0 1)]
           [check-equal? (build-num2 19) '(1 1 0 0 1)])


;; 58
;;; (x 0 y 1) can be 
;;; 8  (0 0 0 1)
;;; 9  (1 0 0 1)
;;; 12 (0 0 1 1)
;;; 13 (1 0 1 1)

;; 59
;;; (x 0 y z) can be
;;; 8  (0 0 0 1)
;;; 9  (1 0 0 1)
;;; 12 (0 0 1 1)
;;; 13 (1 0 1 1)

;; 80
(define poso
  (lambda (n)
    (fresh (a d)
           (== `(,a . ,d) n))))

[check-equal? (run* (q)
                    (poso '(0 1 1))
                    (== #t q))
              '(#t)]

;; 81
[check-equal? (run* (q)
                    (poso '(1))
                    (== #t q))
              '(#t)]

;; 82
[check-equal? (run* (q)
                    (poso '())
                    (== #t q))
              '()]

;; 83
[check-equal? (run* (r)
                    (poso r))
              '((_.0 . _.1))]

;; 86
(define >1o
  (lambda (n)
    (fresh (a ad dd)
           (== `(,a ,ad . ,dd) n))))

[check-equal? (run* (q)
                    (>1o '(0 1 1))
                    (== #t q))
              '(#t)]

;; 87
[check-equal? (run* (q)
                    (>1o '(0 1))
                    (== #t q))
              '(#t)]

;; 88
[check-equal? (run* (q) 
                    (>1o '(1))
                    (== #t q))
              '()]

;; 89
[check-equal? (run* (q)
                    (>1o '())
                    (== #t q))
              '()]

;; 90
[check-equal? (run* (r)
                    (>1o r))
              '((_.0 _.1 . _.2))]

;; 118
(define addero
  (lambda (d n m r)
    (condi
     [(== 0 d) (== '() m) (== n r)]
     [(== 0 d) (== '() n) (== m r) (poso m)]
     [(== 1 d) (== '() m) (addero 0 n '(1) r)]
     [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
     [(== '(1) n) (== '(1) m)
      (fresh (a c)
             (== `(,a ,c) r)
             (full-addero d 1 1 a c))]
     [(== '(1) n) (gen-addero d n m r)]
     [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
     [(>1o n) (gen-addero d n m r)])))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
           (== `(,a . ,x) n)
           (== `(,b . ,y) m) (poso y)
           (== `(,c . ,z) r) (poso z)
           (alli
            (full-addero d a b c e)
            (addero e x y z)))))

(test-case "addero"
           [check-equal? (run 3 (s) 
                              (fresh (x y r)
                                     (addero 0 x y r)
                                     (== `(,x ,y ,r) s)))
                         '((_.0 () _.0)
                           (() (_.0 . _.1) (_.0 . _.1))
                           ((1) (1) (0 1)))]             ;; 96 & 97
           [check-equal? (run 5 (s)
                             (fresh (x y r)
                                    (addero 0 x y r)
                                    (== `(,x ,y ,r) s)))
                        '((_.0 () _.0)
                          (() (_.0 . _.1) (_.0 . _.1))
                          ((1) (1) (0 1))
                          ((1) (0 _.0 . _.1) (1 _.0 . _.1))
                          ((0 _.0 . _.1) (1) (1 _.0 . _.1)))])


;; 120
[check-equal? (run* (s) (gen-addero 1 '(0 1 1) '(1 1) s))
              '((0 1 0 1))]


;; 1
;; 0 1 1
;; 1 1
;; -----
;; 0 1 0 1 


;; 


;; 126
[check-equal? (run* (s)
                    (fresh (x y)
                           (addero 0 x y '(1 0 1))
                           (== `(,x ,y) s)))
              '(((1 0 1) ())
                (() (1 0 1))
                ((1) (0 0 1))
                ((0 0 1) (1))
                ((0 1) (1 1))
                ((1 1) (0 1)))]


;; 128
(define +o 
  (lambda (n m k)
    (addero 0 n m k)))

[check-equal? (run* (s)
                    (fresh (x y)
                           (+o x y '(1 0 1))
                           (== `(,x ,y) s)))
              '(((1 0 1) ())
                (() (1 0 1))
                ((1) (0 0 1))
                ((0 0 1) (1))
                ((0 1) (1 1))
                ((1 1) (0 01)))]


;; 130
(define -o
  (lambda (n m k)
    (+o m k n)))

;; 131
[check-equal? (run* (q)
                    (-o '(0 0 0 1) '(1 0 1) q))
              '((1 1))]

;; 132
[check-equal? (run* (q)
                    (-o '(0 1 1) '(0 1 1) q))
              '(())]

;; 133
[check-equal? (run* (q)
                   (-o '(0 1 1) '(0 0 0 1) q))
              '()]
