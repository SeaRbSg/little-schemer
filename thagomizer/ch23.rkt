#lang racket
(require rackunit)
(require miniKanren)
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
                ((_.0)))]


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

(run* (q)
      (membero 'olive '(olive)))
