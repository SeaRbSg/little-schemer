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
