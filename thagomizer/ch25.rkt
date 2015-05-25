#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(define append 
  (lambda (l s)
    (cond
     ((null? l) s)
     (else (cons (car l) (append (cdr l) s))))))

(test-case "append"
           [check-equal? (append '(a b c) '(d e)) '(a b c d e)]
           [check-equal? (append '(a b c) '()) '(a b c)]
           [check-equal? (append '() '(d e)) '(d e)]
;;           [check-equal? (append 'a '(d e)) '()]
           [check-equal? (append '(d e) 'a) '(d e . a)])


(define appendo
  (lambda (l s out)
    (conde 
     ((nullo l) (== s out))
     (else
      (fresh (a d rest)
             (caro l a)
             (cdro l d)
             (appendo d s rest)
             (conso a rest out))))))

;; 10
[check-equal? (run* (x)
                    (appendo '(cake) '(tastes yummy) x))
              '((cake tastes yummy))]

;; 11
[check-equal? (run* (x)
                    (fresh (y)
                           (appendo `(cake with ice ,y) '(tastes yummy) x)))
                         '((cake with ice _.0 tastes yummy))]

;; 12
[check-equal? (run* (x)
                    (fresh (y)
                           (appendo '(cake with ice cream) y x)))
              '((cake with ice cream . _.0))]

;; 13
[check-equal? (run 1 (x)
                    (fresh (y)
                           (appendo `(cake with ice . ,y) '(d t) x)))
              '((cake with ice d t))]

;; 15
(define appendo2
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d rest)
             (conso a d l)
             (appendo2 d s rest)
             (conso a rest out))))))

;; 16
[check-equal? (run 5 (x)
                    (fresh (y)
                           (appendo2 `(cake with ice . ,y) '(d t) x)))
              '((cake with ice d t)
                (cake with ice _.0 d t)
                (cake with ice _.0 _.1 d t)
                (cake with ice _.0 _.1 _.2 d t)
                (cake with ice _.0 _.1 _.2 _.3 d t))]

;; 17
[check-equal? (run 5 (y)
                   (fresh (x)
                          (appendo2 `(cake with ice . ,y) '(d t) x)))
              '(()
                (_.0)
                (_.0 _.1)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3))]

;; 20
[check-equal? (run 5 (x)
                   (fresh (y)
                          (appendo2 `(cake with ice . ,y)
                                   `(d t . ,y)
                                   x)))
              '((cake with ice d t)
                (cake with ice _.0 d t _.0)
                (cake with ice _.0 _.1 d t _.0 _.1)
                (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
                (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3))]

;; 21
[check-equal? (run* (x)
                    (fresh (z)
                           (appendo2 '(cake with ice cream)
                                    `(d t . ,z)
                                    x)))
              '((cake with ice cream d t . _.0))]

;; 23
[check-equal? (run 6 (x)
                   (fresh (y)
                          (appendo2 x y '(cake with ice d t))))
              '(()
                (cake)
                (cake with)
                (cake with ice)
                (cake with ice d)
                (cake with ice d t))]

;; 25
[check-equal? (run 6 (y)
                   (fresh (x)
                          (appendo2 x y '(cake with ice d t))))
              '((cake with ice d t)
                (with ice d t)
                (ice d t)
                (d t)
                (t)
                ())]

;; 27
[check-equal? (run 6 (r)
                   (fresh (x y)
                          (appendo2 x y '(cake with ice d t))
                          (== `(,x ,y) r)))
              '((() (cake with ice d t))
                ((cake) (with ice d t))
                ((cake with) (ice d t))
                ((cake with ice) (d t))
                ((cake with ice d) (t))
                ((cake with ice d t) ()))]

;; 29
;; Don't run this it never ends
;; [check-equal? (run 7 (r)
;;                    (fresh (x y)
;;                           (appendo2 x y '(cake with ice d t))
;;                           (== `(,x ,y) r)))]

(define appendo3
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d rest)
             (conso a d l)
             (conso a rest out)
             (appendo3 d s rest))))))

;; 32
[check-equal? (run 7 (r)
                   (fresh (x y)
                          (appendo3 x y '(cake with ice d t))
                          (== `(,x ,y) r)))
              '((() (cake with ice d t))
                ((cake) (with ice d t))
                ((cake with) (ice d t))
                ((cake with ice) (d t))
                ((cake with ice d) (t))
                ((cake with ice d t) ()))]

;; 33
[check-equal? (run 7 (x)
                   (fresh (y z)
                          (appendo3 x y z)))
              '(()
                (_.0)
                (_.0 _.1)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3)
                (_.0 _.1 _.2 _.3 _.4)
                (_.0 _.1 _.2 _.3 _.4 _.5))]

;; 34
[check-equal? (run 7 (y)
                   (fresh (x z)
                          (appendo3 x y z)))
              '(_.0 _.0 _.0 _.0 _.0 _.0 _.0)]

;; 36
[check-equal? (run 7 (z)
                   (fresh (x y)
                          (appendo3 x y z)))
              '(_.0
                (_.0 . _.1)
                (_.0 _.1 . _.2)
                (_.0 _.1 _.2 . _.3)
                (_.0 _.1 _.2 _.3 . _.4)
                (_.0 _.1 _.2 _.3 _.4 . _.5)
                (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))]

;; 37
[check-equal? (run 7 (r)
                   (fresh (x y z)
                          (appendo3 x y z)
                          (== `(,x ,y ,z) r)))
              '((() _.0 _.0)
                ((_.0) _.1 (_.0 . _.1))
                ((_.0 _.1) _.2 (_.0 _.1 . _.2))
                ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
                ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
                ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
                ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))
                )]

;; 38
(define swappendo
  (lambda (l s out)
    (conde
     (s# 
      (fresh (a d rest)
             (conso a d l)
             (conso a rest out)
             (swappendo d s rest)))
     (else (nullo l) (== s out)))))

;; 39 Do not run, never finishes
;; (run 1 (z)
;;      (fresh (x y)
;;             (swappendo x y z)))

;; 41
(define unwrap
  (lambda (x)
    (cond
     ((pair? x) (unwrap (car x)))
     (else x))))

[check-equal? (unwrap '((((pizza))))) 'pizza]


;; 45
(define unwrapo
  (lambda (x out)
    (conde 
     ((pairo x) 
      (fresh (a)
             (caro x a)
             (unwrapo a out)))
     (else (== x out)))))


;; 46
[check-equal? (run* (x)
                    (unwrapo '(((pizza))) x))
              '(pizza
                (pizza)
                ((pizza))
                (((pizza))))]

;; 48
;; [check-equal? (run 1 (x)
;;                    (unwrapo x 'pizza))
;;               '()]

;; 49
;; [check-equal? (run 1 (x) (unwrapo '((x)) 'pizza)) '()]

;; 52
(define unwrapo2
  (lambda (x out)
    (conde
     (s# (== x out))
     (else
      (fresh (a)
             (caro x a)
             (unwrapo2 a out))))))

;; 53
[check-equal? (run 5 (x)
                   (unwrapo2 x 'pizza))
              '(pizza
                (pizza . _.0)
                ((pizza . _.0) . _.1)
                (((pizza . _.0) . _.1) . _.2)
                ((((pizza . _.0) . _.1) . _.2) . _.3))]

;; 54
[check-equal? (run 3 (x)
                   (unwrapo2 x '((pizza))))
              '(((pizza))
                (((pizza)) . _.0)
                ((((pizza)) . _.0) . _.1))]

;; 55
[check-equal? (run 3 (x)
                   (unwrapo2 `((,x)) 'pizza))
              '(pizza
                (pizza . _.0)
                ((pizza . _.0) . _.1))]

;; 58
(define flatten
  (lambda (s)
    (cond
     [(null? s) '()]
     [(pair? s)
      (append
       (flatten (car s))
       (flatten (cdr s)))]
     [else (cons s '())])))
[check-equal? (flatten '((a b) c)) '(a b c)]

;; 59
(define flatteno
  (lambda (s out)
    (conde
     [(nullo s) (== '() out)]                 ;; 59a
     [(pairo s)                               ;; 59b
      (fresh (a d res-a res-d)
             (conso a d s)
             (flatteno a res-a)
             (flatteno d res-d)
             (appendo3 res-a res-d out))]
     [else (conso s '() out)])))              ;; 59c

;; 60
[check-equal? (run 1 (x)
                   (flatteno '((a b) c) x))
              '((a b c))]

;; 61
[check-equal? (run 1 (x)
                   (flatteno '(a (b c)) x))
              '((a b c))]

;; 62
[check-equal? (run* (x)
                    (flatteno '(a) x))
              '((a)      ;; 59b and 59a
                (a ())   ;; 59b and 59c
                ((a)))]  ;; 59c

;; 64
[check-equal? (run* (x) (flatteno '((a)) x))
              '((a)
                (a ())
                (a ())
                (a () ())
                ((a))
                ((a) ())
                (((a))))]

;; 66
;; s = (((a)))
;; out = (

;;       )

;; ;; 59a is skipped
;; ;; 59b
;; a = ((a))
;; d = ()
;; ;; from previous problems I know
;; res-a = (a) (a ()) (a ()) (a () ()) ((a)) ((a) ()) (((a))) 
;; res-d = () (())

;; out = (a)              ;; res-a[0] res-d[0]
;; out = (a ())           ;; res-a[0] res-d[1]
;; out = (a ())           ;; res-a[1] res-d[0]
;; out = (a () ())        ;; res-a[1] res-d[1]
;; out = (a ())           ;; res-a[2] res-d[0]
;; out = (a () ())        ;; res-a[2] res-d[1]
;; out = (a () ())        ;; res-a[3] res-d[0]
;; out = (a () () ())     ;; res-a[3] res-d[1]
;; out = ((a))            ;; res-a[4] res-d[0]
;; out = ((a) ())         ;; res-a[4] res-d[1]
;; out = ((a) ())         ;; res-a[5] res-d[0]
;; out = ((a) () ())      ;; res-a[5] res-d[1]
;; out = (((a)))          ;; res-a[6] res-d[0]
;; out = (((a)) ())       ;; res-a[6] res-d[1]

;; ;; 59c
;; out = ((((a))))
[check-equal? (run* (x) (flatteno '(((a))) x))
              '((a)            
                (a ())         
                (a ())
                (a () ())
                (a ())
                (a () ())
                (a () ())
                (a () () ())
                ((a))
                ((a) ())
                ((a) ())
                ((a) () ())
                (((a)))
                (((a)) ())
                ((((a)))))]

;; 68
[check-equal? (run* (x) (flatteno '((a b) c) x))
              '((a b c)
                (a b c ())
                (a b (c))
                (a b () c)
                (a b () c ())
                (a b () (c))
                (a (b) c)
                (a (b) c ())
                (a (b) (c))
                ((a b) c)
                ((a b) c ())
                ((a b) (c))
                (((a b) c)))]

;; Notes for frame 68
;; s = ((a b) c)
;; out = ?

;; ;; 59a is skipped
;; ;; 59b
;; a = (a b)
;; d = (c)

;; --------------
;; ;; flatteno (a b)
;; s = (a b)
;; out = ((a b) (a b ()) (a (b)) ((a b)))

;; ;; 59a is skipped
;; ;; 59b
;; a = a
;; d = (b)
;; res-a = (a)
;; res-d = (b) (b ()) ((b)) ;; from frame 62

;; out = (a b)
;; out = (a b ())
;; out = (a (b))

;; ;; 59c
;; out = ((a b))

;; --------------
;; ;; flatteno (c)
;; s = (c)
;; out = (c) (c ()) ((c))

;; ;; 59a is skipped
;; ;; 59b
;; a = c
;; d = ()
;; res-a = (c)
;; res-d = (() (()))
;; out = (c) 
;; out = (c ())

;; ;; 59c 
;; out = ((c))

;; --------------
;; ;; flatteno ((a b) c) resumed
;; res-a = ((a b) (a b ()) (a (b)) ((a b)))
;; res-d = ((c) (c ()) ((c)))

;; out = (a b c)
;; out = (a b c ())
;; out = (a b (c))
;; out = (a b () c)
;; out = (a b () c ())
;; out = (a b () (c))
;; out = (a (b) c)
;; out = (a (b) c ())
;; out = (a (b) (c))
;; out = ((a b) c)
;; out = ((a b) c ())
;; out = ((a b) (c))

;; ;; 59c
;; out = (((a b) c))

;; 73
(define flattenrevo
  (lambda (s out)
    (conde 
     [s# (conso s '() out)]            ;; 73a
     [(nullo s) (== '() out)]          ;; 73b
     [else                             ;; 73c
      (fresh (a d res-a res-d)
             (conso a d s)
             (flattenrevo a res-a)
             (flattenrevo d res-d)
             (appendo3 res-a res-d out))])))


;; 75
[check-equal? (run* (x) (flattenrevo '((a b) c) x))
              '((((a b) c)) 
                ((a b) (c)) 
                ((a b) c ())
                ((a b) c)
                (a (b) (c))
                (a (b) c ())
                (a (b) c)
                (a b () (c))
                (a b () c ())
                (a b () c)
                (a b (c))
                (a b c ())
                (a b c))]

;; 76
[check-equal? (reverse (run* (x) (flattenrevo '((a b) c) x)))
              '((a b c)
                (a b c ())
                (a b (c))
                (a b () c)
                (a b () c ())
                (a b () (c))
                (a (b) c)
                (a (b) c ())
                (a (b) (c))
                ((a b) c)
                ((a b) c ())
                ((a b) (c))
                (((a b) c)))]

;; 77
;; TO DISCUSS: I HAVE NO IDEA HOW THIS COMES UP WITH THE ANSWER
[check-equal? (run 2 (x)
                   (flattenrevo x '(a b c)))
              '((a b . c)
                (a b c))]

;; 79
;; Do not run infinite
;; [check-equal? (run 3 (x)
;;                    (flattenrevo x '(a b c)))
;;               '((a b . c)
;;                 (a b c))]

;; 80
[check-equal? (length (run* (x)
                            (flattenrevo '((((a (((b))) c))) d) x)))
              574]
