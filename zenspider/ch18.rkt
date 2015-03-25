;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit))

(require "lib/shared.rkt")

(define (bons kar)
  (let ((kdr '()))
    (lambda (selector)
      (selector (lambda (x) (set! kdr x)) kar kdr))))

;; (define (kons kar kdr)
;;   (lambda (selector)
;;     (selector kar kdr)))

(define (kar c)
  (c (lambda (s a d) a)))

(define (kdr c)
  (c (lambda (s a d) d)))

(define (set-kdr c x)
  ((c (lambda (s a d) s)) x))

(define (kons a d)
  (let ((c (bons a)))
    (set-kdr c d)
    c))

(define k #f)
(define (set-kounter n)
  (set! k n))
(set-kounter 0)
(define (kounter) k)

(set-kounter (kounter))                 ; hack to force coverage before konsC

(define konsC
  (let ((N 0))
    (set! kounter
          (lambda ()
            N))
    (set! set-kounter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lots m)
  (cond [(zero? m) '()]
        [else (kons 'egg (lots (sub1 m)))]))

(define (lenkth l)
  (cond [(null? l) 0]
        [else (add1 (lenkth (kdr l)))]))

(define (add-at-end l)
  (cond [(null? (kdr l))
         (konsC (kar l) (kons 'egg '()))]
        [else (konsC (kar l) (add-at-end (kdr l)))]))

(define (add-at-end-too l)
  (letrec ((A (lambda (ls)
                (cond [(null? (kdr ls))
                       (set-kdr ls (kons 'egg '()))]
                      [else (A (kdr ls))]))))
    (A l)
    l))

(define (kons->list l)
  (cond [(null? l) '()]
        [else (cons (kar l)
                    (kons->list (kdr l)))]))

(module+ test
  (check-equal? (kons->list (lots 3))
                '(egg egg egg))
  (check-equal? (lenkth (lots 3))
                3)

  (set-kounter 0)
  (check-equal? (kons->list (add-at-end (lots 3)))
                '(egg egg egg egg))
  (check-equal? (kounter)
                3)

  (set-kounter 0)
  (check-equal? (kons->list (add-at-end-too (lots 3)))
                '(egg egg egg egg))
  (check-equal? (kounter)
                0))
