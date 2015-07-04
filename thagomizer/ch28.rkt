#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch27.rkt")

;; 10
(define *o
  (lambda (n m p)
    (condi
     [(== '() n) (== '() p)]
     [(poso n) (== '() m) (== '() p)]
     [(== '(1) n) (poso m) (== m p)]
     [(>1o n) (== '(1) m) (== n p)]
     [(fresh (x z)
             (== `(0 . ,x) n)
             (poso x)
             (== `(0 . ,z) p)
             (poso z)
             (>1o m)
             (*o x m z))]
     [(fresh (x y)
             (== `(1 . ,x) n)
             (poso x)
             (== `(0 . ,y) m)
             (poso y)
             (*o m n p))]
     [(fresh (x y)
             (== `(1 . ,x) n)
             (poso x)
             (== `(1 . ,y) m) 
             (poso y)
             (odd-*o x n m p))]
     [else u#])))

;; 18
(define odd-*o
  (lambda (x n m p)
    (fresh (q)
           (bound-*o q p n m)
           (*o x m q)
           (+o `(0 . ,q) m p))))

;; 22
(define bound-*o
  (lambda (q p n m)
    (conde
     [(nullo q) (pairo p)]
     [else
      (fresh (x y z)
             (cdro q x)
             (cdro p y)
             (condi 
              [(nullo n)
               (cdro m z)
               (bound-*o x y z '())]
              [else 
               (cdro n z)
               (bound-*o x y z m)]))])))

;; 20
[check-equal? (run 1 (t)
                   (fresh (n m)
                          (*o n m '(1))
                          (== `(,n ,m) t)))
              '(((1) (1)))]

;; 23
[check-equal? (run 2 (t)
                   (fresh (n m)
                          (*o n m '(1))
                          (== `(,n ,m) t)))
              '(((1) (1)))]

;; 24
[check-equal? (run* (p)
                    (*o '(1 1 1) '(1 1 1 1 1 1) p))
              '((1 0 0 1 1 1 0 1 1))]

;; 26
(define =lo
  (lambda (n m)
    (conde
     [(== '() n) (== '() m)]
     [(== '(1) n) (== '(1) m)]
     [else
      (fresh (a x b y)
             (== `(,a . ,x) n)
             (poso x)
             (== `(,b . ,y) m)
             (poso y)
             (=lo x y))])))

;; 27
[check-equal? (run* (t)
                    (fresh (w x y)
                           (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))
                           (== `(,w ,x ,y) t)))
              '((_.0 _.1 (_.2 1)))]

;; 28
[check-equal? (run* (b)
                    (=lo '(1) `(,b)))
              '(1)]

;; 29
[check-equal? (run* (n)
                    (=lo `(1 0 1 . ,n) '(0 1 1 0 1)))
              '((_.0 1))]

;; 30
[check-equal? (run 5 (t)
                   (fresh (y z)
                          (=lo `(1 . ,y) `(1 . ,z))
                          (== `(,y ,z) t)))
              '((()              ())
                ((1)             (1))
                ((_.0 1)         (_.1 1))
                ((_.0 _.1 1)     (_.2 _.3 1))
                ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1)))]

;; 31
[check-equal? (run 5 (t)
                   (fresh (y z)
                          (=lo `(1 . ,y) `(0 . ,z))
                          (== `(,y ,z) t)))
              '(((1) (1))
                ((_.0 1) (_.1 1))
                ((_.0 _.1 1) (_.2 _.3 1))
                ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
                ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1)))]

;; 33
[check-equal? (run 5 (t)
                   (fresh (y z)
                          (=lo `(1 . ,y) `(0 1 1 0 1 . ,z))
                          (== `(,y ,z) t)))
              '(((_.0 _.1 _.2 1) ())
                ((_.0 _.1 _.2 _.3 1) (1))
                ((_.0 _.1 _.2 _.3 _.4 1) (_.5 1))
                ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 1))
                ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 1) (_.7 _.8 _.9 1)))]

;; 34
(define <lo 
  (lambda (n m)
    (conde
     [(== '() n) (poso m)]
     [(== '(1) n) (>1o m)]
     [else
      (fresh (a x b y)
             (== `(,a . ,x) n)
             (poso x)
             (== `(,b . ,y) m) 
             (poso y)
             (<lo x y))])))

;; 35
[check-equal? (run 8 (t)
                   (fresh (y z)
                          (<lo `(1 . ,y) `(0 1 1 0 1 . ,z))
                          (== `(,y ,z) t)))
              '((() _.0)
                ((1) _.0)
                ((_.0 1) _.1)
                ((_.0 _.1 1) _.2)
                ((_.0 _.1 _.2 1) (_.3 . _.4))
                ((_.0 _.1 _.2 _.3 1) (_.4 _.5 . _.6))
                ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 . _.8))
                ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 . _.10)))]

;; 38
(define <=lo 
  (lambda (n m)
    (conde
     [(=lo n m) s#]
     [(<lo n m) s#])))

;; 39
[check-equal? (run 8 (t)
                   (fresh (n m)
                          (<=lo n m)
                          (== `(,n ,m) t)))
              '((() ())
                ((1) (1))
                ((_.0 1) (_.1 1))
                ((_.0 _.1 1) (_.2 _.3 1))
                ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
                ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1))
                ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 _.8 _.9 1))
                ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 _.10 _.11 1)))]

;; 40
[check-equal? (run 1 (t)
                    (fresh (n m)
                           (<=lo n m)
                           (*o n '(0 1) m)
                           (== `(,n ,m) t)))
              '((() ()))]

;; 42
(define <=lo2
  (lambda (n m)
    (condi
     [(=lo n m) s#]
     [(<lo n m) s#])))

;; 43
[check-equal? (run 10 (t)
                   (fresh (n m)
                          (<=lo2 n m)
                          (*o n '(0 1) m)
                          (== `(,n ,m) t)))
              '((() ())
                ((1) (0 1))
                ((0 1) (0 0 1))
                ((1 1) (0 1 1))
                ((0 0 1) (0 0 0 1))
                ((1 _.0 1) (0 1 _.0 1))
                ((0 1 1) (0 0 1 1))
                ((0 0 0 1) (0 0 0 0 1))
                ((1 _.0 _.1 1) (0 1 _.0 _.1 1))
                ((0 1 _.0 1) (0 0 1 _.0 1)))]

;; 44
[check-equal? (run 6 (t)
                   (fresh (n m)
                          (<=lo2 n m)
                          (== `(,n ,m) t)))
              '((() ())
                (() (_.0 . _.1))
                ((1) (1))
                ((1) (_.0 _.1 . _.2))
                ((_.0 1) (_.1 1))
                ((_.0 1) (_.1 _.2 _.3 . _.4)))]

;; 46
(define <o
  (lambda (n m)
    (condi 
     [(<lo n m) s#]
     [(=lo n m)
      (fresh (x)
             (poso x)
             (+o n x m))])))

(define <=o
  (lambda (n m)
    (condi
     [(== n m) s#]
     [(<o n m) s#])))

;; 47
[check-equal? (run* (q)
                    (<o '(1 0 1) '(1 1 1))
                    (== #t q))
              '(#t)]

;; 48
[check-equal? (run* (q)
                    (<o '(1 1 1) '(1 0 1))
                    (== #t q))
              '()]

;; 49
[check-equal? (run* (q)
                    (<o '(1 0 1) '(1 0 1))
                    (== #t q))
              '()]

;; 50
[check-equal? (run 6 (n)
                   (<o n '(1 0 1)))
              '(()
                (0 0 1)
                (1)
                (_.0 1))]

;; 51
[check-equal? (run 6 (m)
                   (<o '(1 0 1) m))
              '((_.0 _.1 _.2 _.3 . _.4)
                (0 1 1)
                (1 1 1))]

;; 52
;; [check-equal? (run* (n)
;;                     (<o n n))]


;; 63
(define ÷o
  (lambda (n m q r)
    (condi
     [(== '() q) (== n r) (<o n m)]
     [(== '(1) q) (== '() r) (== n m)
      (<o r m)]
     [(<o m n) (<o r m)
      (fresh (mq)
             (<=lo mq n)
             (*o m q mq)
             (+o mq r n))])))

;; 71
[check-equal? (run* (m)
                    (fresh (r)
                           (÷o '(1 0 1) m '(1 1 1) r)))
              '()]

;; 76
(define ÷o2
  (lambda (n m q r)
    (condi
     [(== r n) (== '() q) (<o n m)]
     [(== '(1) q) (=lo n m) (+o r m n) (<o r m)]
     [else
      (alli
       (<lo m n)
       (<o r m)
       (poso q)
       (fresh (nh nl qh ql qlm qlmr rr rh)
              (alli
               (splito n r nl nh)
               (splito q r ql qh)
               (conde
                [(== '() nh) (== '() qh) (-o nl r qlm) (*o ql m qlm)]
                [else
                 (alli
                  (poso nh)
                  (*o ql m qlm)
                  (+o qlm r qlmr)
                  (-o qlmr nl rr)
                  (splito rr r '() rh)
                  (÷o nh m qh rh))]))))])))

(define splito 
  (lambda (n r l h)
    (condi
     [(== '() n) (== '() h) (== '() l)]
     [(fresh (b n-hat)
             (== `(0 ,b . ,n-hat) n)
             (== '() r)
             (== `(,b . ,n-hat) h)
             (== '() l))]
     [(fresh (n-hat)
             (== `(1 . ,n-hat) n)
             (== '() r)
             (== n-hat h)
             (== '(1) l))]
     [(fresh (b n-hat a r-hat)
             (== `(0 ,b . ,n-hat) n)
             (== `(,a . ,r-hat) r)
             (== '() l)
             (splito `(,b . ,n-hat) r-hat '() h))]
     [(fresh (n-hat a r-hat)
             (== `(1 . ,n-hat) n)
             (== `(,a . ,r-hat) r)
             (== '(1) l)
             (splito n-hat r-hat '() h))]
     [(fresh (b n-hat a r-hat l-hat)
             (== `(,b . ,n-hat) n)
             (== `(,a . ,r-hat) r)
             (== `(,b . ,l-hat) l)
             (poso l-hat)
             (splito n-hat r-hat l-hat h))])))


[check-equal? (run* (q)
                    (fresh (l h)
                           (splito '(0 0 1) '() l h)
                           (== `(,l ,h) q)))
              '((() (0 1)))]

[check-equal? (run* (q)
                    (fresh (l h)
                           (splito '(1 0 1) '() l h)
                           (== `(,l ,h) q)))
              '(((1) (0 1)))]

[check-equal? (run* (q)
                    (fresh (l h)
                           (splito '(1 0 1 0 1) '(1) l h)
                           (== `(,l ,h) q)))
              '(((1) (1 0 1)))]

[check-equal? (run* (q)
                    (fresh (l h)
                           (splito '(1 0 1 0 1) '(0 1) l h)
                           (== `(,l ,h) q)))
              '(((1 0 1) (0 1)))]

;; 81
;; [check-equal? (run 3 (t)
;;                    (fresh (y z)
;;                           (÷o `(1 0 . ,y) '(0 1) z '())
;;                           (== `(,y ,z) t)))
;;               '()]


;; 
