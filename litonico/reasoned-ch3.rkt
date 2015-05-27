;; I GET IT! 'd' is the 'decrement' part, like in 'cdr',
;; and 'a' is the 'address' part, like in 'car'.
#lang racket

(require "../lib/mk.rkt")
(require rackunit)
(require "reasoned-prelude.rkt")

[check-equal?
  (list? '(d a t e . s))
  #f]

(define listo
  (lambda (l)
    (conde
      [(nullo l) %s]
      [(pairo l)
       (fresh (tail)
        (cdro l tail)
        (listo tail))]
      [else %u])))

[check-equal?
  (run* (x)
    (listo `(a b ,x d)))
  '(_.0)]

[check-equal?
  (run 1 (x)
    (listo `(a b c . ,x)))
  '(())]


;; (define nope
;;   (run* (x)
;;     (listo `(a b c . ,x))))

(run 5 (x)
  (listo `(a b c . ,x)))

(define lol? ;; list-of-lists?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(list? (car l)) (lol? cdr l)]
      [else #f])))

(define lolo
  (lambda (l)
    (conde
      [(nullo l) %s]
      [(fresh (head)
              (caro l head)
              (listo head))
       (fresh (tail)
              (cdro l tail)
              (lolo tail))]
      [else %u])))

[check-equal?
  (run 1 (l)
    (lolo l))
  '(())]


(run 5 (x)
  (lolo `((a b) (c d) . ,x)))

; (define my-twinso ;; Why doesn't this work?
;   (lambda (s)
;     (fresh (head)
;            (conso head head s))))

;; Oh, because '(tofu . tofu) is not a twin; '(tofu tofu) is.

(define twinso
  (lambda (s)
    (fresh (x y)
      (conso x y s)
      (conso x '() y))))

[check-equal?
  (run* (q)
    (twinso '(tofu tofu))
    (== #t q))
  '(#t)]

[check-equal?
  (run* (x)
    (twinso `(,x tofu)))
  '(tofu)]

(define simpler-twinso ;; I like this better
  (lambda (s)
    (fresh (head)
           (== (head head) s))))

(define loto
  (lambda (l)
    (conde
      [(nullo l) %s]
      [(fresh (head)
              (caro l head)
              (twinso head))
       (fresh (tail)
              (cdro l tail)
              (loto tail))]
      [else %u])))

(run 5 (z)
      (loto `((g g) . ,z)))


;; Question for study group: WTF?
(run 5 (z) ;; This is just confusing
      (fresh (w x y z)
             (loto `((g g) (e ,w) (,x ,y) . ,z))
             (== `(,w (,x ,y) ,z) z)))

(define listofo
  (lambda (predo l)
    (conde
      [(nullo l) %s]
      [(fresh (head)
              (caro l head)
              (predo head))
       (fresh (tail)
              (cdro l tail)
              (listofo predo tail))]
      [else %u])))

(define clean-loto
  (lambda (l)
    (listofo twinso l)))

(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) #t]
      [else (member? x (cdr l))])))

(define eq-car?
  (lambda (l x)
    (eq? x (car l))))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
      [(eq-caro l x) %s]
      [else
        (fresh (d)
               (cdro l d)
               (membero x d))])))

(define identity
  (lambda (l)
    (run* (y)
          (== y l))))

(define first-value
  (lambda (l)
    (run 1 (y)
      (membero y l))))

(define memberrevo
  (lambda (x l)
    (conde
      [(nullo l) %u]
      [(fresh (d)
              (cdro l d)
              (memberrevo x d))]
      [else (eq-caro l x)])))

(define reverse-list
  (lambda (l)
    (run* (y)
      (memberrevo y l))))
