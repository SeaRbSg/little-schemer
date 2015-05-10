#lang racket
(require miniKanren)

(provide else s# u# caro cdro conso nullo eqo pairo listo)

(define else (lambda x #f)) ;; This feels very very dirty
(define s# (== #f #f)) ;; succeed
(define u# (== #f #t)) ;; fail

;; caro is a function of two variables 
;;  p is a list
;;  a is an atom
;;  caro succeeds when a is the car of p
(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

;; cdro is a function of two variables
;;   p is a list
;;   d is a list
;;   cdro succeeds when d is the cdr of p
(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))


(define conso
  (lambda (f r l)
    (== (cons f r) l)))


(define nullo
  (lambda (x)
    (== '() x)))


(define eqo
  (lambda (n m)
    (== n m)))


(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))


(define listo
  (lambda (l)
    (conde
     ((nullo l) s#)
     ((pairo l)
      (fresh (d)
             (cdro l d)
             (listo d)))
     (else u#))))
