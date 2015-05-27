#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

[check-true  (list? '((a) (a b) c))]

[check-true  (list? '())]

[check-false (list? 's)]

[check-false (list? `(d a t e . s))]

[check-equal? (run* (x)
                    (listo `(a b ,x d)))
              '(_.0)]

[check-equal? (run 1 (x)
                    (listo `(a b c . ,x)))
              '(())]

[check-equal? (run 3 (x)
                    (listo `(a b c . ,x)))
              '(() (_.0) (_.0 _.1))]

(define lol?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((list? (car l)) (lol? (cdr l)))
     (else #f))))

(define lolo
  (lambda (l)
    (conde
     ((nullo l) s#)
     ((fresh (a d)
             (caro l a)
             (listo a)
             (cdro l d)
             (lolo d)))
     (else u#))))

[check-equal? (run 1 (l)
                    (lolo l))
              '(())]
 

[check-equal? (run* (q)
                    (fresh (x y)
                           (lolo `((a b) (,x c) (d y)))
                           (== #t q)))
              '(#t)]


[check-equal? (run 1 (q)
                   (fresh (x)
                          (lolo `((a b) . ,x))
                          (== #t q)))
              '(#t)]


[check-equal? (run 1 (x) 
                   (lolo `((a b) (c d) . ,x)))
              '(())]


;; DISCUSS This I don't get, also why is it different than the book
[check-equal? (run 3 (x)
                   (lolo `((a b) (c d) . ,x)))
              '(()
                (())
                (() ()))]


(define twin 
  (lambda (l)
    (and (eq? (length l) 2)
         (eq? (car l) (cadr l)))))

(define list-of-twins
  (lambda (l)
    (cond
     [(null? l) #t]
     [(and (twin (car l))
           (list-of-twins (cdr l)))]
     [else #f])))

[check-true  (twin '(tofu tofu))]
[check-false (twin '(e tofu))]
[check-false (twin '(g g g))]
[check-true  (list-of-twins '((g g) (tofu tofu)))]
[check-false (list-of-twins '((g g) (e tofu)))]

(define twinso
  (lambda (s)
    (fresh (x y)
           (conso x y s)
           (conso x '() y))))

;; s is a list of twins if x and y cons'd together make s AND
;; if x cons'd with '() makes y.

[check-equal? (run* (q)
                    (twinso '(tofu tofu))
                    (== #t q))
              '(#t)]

[check-equal? (run* (z) 
                    (twinso `(,z tofu)))
              '(tofu)]

(define twinso1
  (lambda (s)
    (fresh (x)
           (== `(,x ,x) s))))

[check-equal? (run* (q)
                    (twinso1 '(tofu tofu))
                    (== #t q))
              '(#t)]

[check-equal? (run* (z) 
                    (twinso1 `(,z tofu)))
              '(tofu)]


(define loto
  (lambda (l)
    (conde
     [(nullo l) s#]
     [(fresh (a)
             (caro l a)
             (twinso a))
      (fresh (d)
             (cdro l d)
             (loto d))]
     [else u#])))

[check-equal? (run 1 (z)
                   (loto `((g g) . ,z)))
              '(())]

[check-equal? (run 5 (z)
                   (loto `((g g) . ,z)))
              '(()
                ((_.0 _.0))
                ((_.0 _.0) (_.1 _.1))
                ((_.0 _.0) (_.1 _.1) (_.2 _.2))
                ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3)))]


[check-equal? (run 3 (r)
                   (fresh (w x y z)
                          (loto `((g g) (e ,w) (,x ,y) . ,z))
                          (== `(,w (,x ,y) ,z) r)))
              '((e (_.0 _.0) ())
                (e (_.0 _.0) ((_.1 _.1)))
                (e (_.0 _.0) ((_.1 _.1) (_.2 _.2))))]


[check-equal? (run 3 (out)
                   (fresh (w x y z)
                          (== `((g g) (e ,w) (,x ,y) . ,z) out)
                          (loto out)))
              '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]


(define listofo
  (lambda (predo l)
    (conde 
     [(nullo l) s#]
     [(fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))]
     [else u#])))


[check-equal? (run 3 (out)
                   (fresh (w x y z)
                          (== `((g g) (e ,w) (,x ,y) . ,z) out)
                          (listofo twinso out)))
              '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]


(define loto2
  (lambda (l)
    (listofo twinso l)))


(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define member?
  (lambda (x l)
    (cond
     [(null? l) #f]
     [(eq-car? l x) #t]
     [else (member? x (cdr l))])))


[check-true (member? 'olive '(virgin olive oil))]


(define eq-caro
  (lambda (l x)
    (caro l x)))

[check-equal? (run 1 (x)
                   (eq-caro '(olive) 'oil)
                   (== #t x))
              '()]

(define membero
  (lambda (x l)
    (conde 
     [(eq-caro l x) s#]
     [(nullo '())
      (fresh (d)
             (cdro l d)
             (membero x d))])))

[check-equal? (run* (q)
                     (membero 'olive '(virgin olive oil))
                     (== #t q))
              '(#t)]
[check-equal? (run 1 (y)
                   (membero y '(hummus with pita)))
              '(hummus)]

[check-equal? (run 1 (y)
                   (membero y '(with pita)))
              '(with)]

[check-equal? (run 1 (y)
                   (membero y '(pita)))
              '(pita)]

[check-equal? (run* (y)
                    (membero y '()))
              '()]

[check-equal? (run* (y)
                    (membero y '(hummus with pita)))
              '(hummus with pita)]


(define identity
  (lambda (l)
    (run* (y)
          (membero y l))))

(test-case "identity"
           [check-equal? (identity '()) '()]
           [check-equal? (identity '(a)) '(a)]
           [check-equal? (identity '(a b c)) '(a b c)])


[check-equal? (run* (x)
                    (membero 'e `(pasta ,x fagioli)))
              '(e)]

;; To Discuss: 69
[check-equal? (run 1 (x)
                   (membero 'e `(pasta e ,x fagioli)))
              '(_.0)]

[check-equal? (run 1 (x)
                   (membero 'e `(pasta ,x e fagioli)))
              '(e)]

[check-equal? (run* (r)
                    (fresh (x y)
                           (membero 'e `(pasta ,x fagioli ,y))
                           (== `(,x ,y) r)))
              '((e _.0) (_.0 e))]

[check-equal? (run 1 (l)
                   (membero 'tofu l))
              '((tofu . _.0))]

;; Runs forever
;; [check-equal? (run* (l)
;;                     (membero 'tofu l))
;;               '()]


[check-equal? (run 5 (l)
                   (membero 'tofu l))
              '((tofu . _.0) 
                (_.0 tofu . _.1)
                (_.0 _.1 tofu . _.2)
                (_.0 _.1 _.2 tofu . _.3)
                (_.0 _.1 _.2 _.3 tofu . _.4))]


(define pmembero
  (lambda (x l)
    (conde
     [(nullo l) u#]
     [(eq-caro l x) (cdro l '())]
     [else
      (fresh (d)
             (cdro l d)
             (pmembero x d))])))

[check-equal? (run 5 (l)
                   (pmembero 'tofu l))
              '((tofu) 
                (_.0 tofu)
                (_.0 _.1 tofu)
                (_.0 _.1 _.2 tofu)
                (_.0 _.1 _.2 _.3 tofu))]

[check-equal? (run* (q)
                    (pmembero 'tofu '(a b tofu d tofu))
                    (== #t q))
              '(#t)]


(define pmembero1
  (lambda (x l)
    (conde 
     [(nullo l) u#]                ;; 1
     [(eq-caro l x) (cdro l '())]  ;; 2
     [(eq-caro l x) s#]            ;; 3
     [else             
      (fresh (d)
             (cdro l d)
             (pmembero1 x d))])))

[check-equal? (run* (q)
                    (pmembero1 'tofu '(a b tofu d tofu))
                    (== #t q))
              '(#t #t #t)] 
;; The first tofu matches line 3, the second tofu matches lines 2 and line 3. So three trues

(define pmembero2
  (lambda (x l)
    (conde
     [(eq-caro l x) (cdro l '())]      ;; 1
     [(eq-caro l x)                    ;; 2
      (fresh (a d)
             (cdro l `(,a . ,d)))]
     [else                             ;; 3
      (fresh (d)
             (cdro l d)
             (pmembero2 x d))])))

[check-equal? (run* (q)
                    [pmembero2 'tofu '(a b tofu d tofu)]
                    (== #t q))
              '(#t #t)]

[check-equal? (run 4 (l)
                   (pmembero2 'tofu l))
              '((tofu)
                (tofu _.0 . _.1)
                (_.0 tofu)
                (_.0 tofu _.1 . _.2))]
;; (tofu) comes from clause 1
;; (tofu _.0 . _1) comes from clause 2, the head is tofu and the rest is two fresh variables dotted together
;; (_.0 tofu) comes from the 3rd and 1st clauses (recursion)
;; (_.0 tofu _.1 . _.2) comes from the 3rd and 2nd clauses


(define pmembero3
  (lambda (x l)
    (conde
     [(eq-caro l x)                    ;; 1
      (fresh (a d)
             (cdro l `(,a . ,d)))]
     [(eq-caro l x) (cdro l '())]      ;; 2
     [else                             ;; 3
      (fresh (d)
             (cdro l d)
             (pmembero3 x d))])))

[check-equal? (run 4 (l)
                   (pmembero3 'tofu l))
              '((tofu _.0 . _.1)
                (tofu)
                (_.0 tofu _.1 . _.2)
                (_.0 tofu))]


(define first-value
  (lambda (l)
    (run 1 (y)
         (membero y l))))

[check-equal? (first-value '(pasta e fagioli))
              '(pasta)]


(define memberrevo
  (lambda (x l)
    (conde 
;;     [(nullo l) u#]
     [s# (fresh (d)
                (cdro l d)
                (memberrevo x d))]
     [else (eq-caro l x)])))


[check-equal? (run* (x)
                    (memberrevo x '(pasta e fagioli)))
              '(fagioli e pasta)]


(define reverse-list
  (lambda (l)
    (run* (y)
          (memberrevo y l))))
