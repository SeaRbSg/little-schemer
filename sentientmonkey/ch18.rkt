#lang racket/base

(require rackunit)
(require "prelude.rkt")

(define counter 0)
(define set-counter 0)

(define consC
  (let ([N 0])
    (set! counter (lambda () N))
    (set! set-counter (lambda (n) (set! N n)))
    (lambda (a b)
      (set! N (add1 N))
      (cons a b))))

(define (lots m)
  (if (zero? m)
    '()
    (cons 'egg (lots (sub1 m)))))

(test-case "lots"
  (check-equal? (lots 3) '(egg egg egg))
  (check-equal? (lots 5) '(egg egg egg egg egg))
  (check-equal? (lots 12) '(egg egg egg egg egg egg egg egg egg egg egg egg)))

(define (lenkth l)
  (if (null? l)
    0
    (add1 (lenkth (cdr l)))))

(test-case "lenkth"
  (check-equal? (lenkth (lots 5)) 5)
  (check-equal? (lenkth (lots 5)) 5))

(test-case "4 eggs" (check-equal? (cons 'egg (lots 3)) '(egg egg egg egg)))

; this was my take on add-at-end
(define (add-at-end l)
  (if (null? l)
    (consC 'egg '())
    (consC (car l) (add-at-end (cdr l)))))

(test-case "add-at-end"
  (check-equal? (add-at-end (lots 3)) '(egg egg egg egg))
  (check-equal? (counter) 4))

; My implementation counter is 4 and not 3. But my version is simplier and handles the null check.

; doesn't run because we don't have set-kdr :(
; (define (add-at-end-too l)
;   (letrec
;     ([A (lambda (ls)
;           (if (null? (kdr ls))
;             (set-kdr ls (kons 'egg '()))
;             (A (kdr l))))])
;     (A l)
;     l))

; lst form of kons, kar, & kdr
; (define (kons kar kdr)
;   (lambda (selector)
;     (selector kar kdr)))
;
; (define (kar c)
;   (c (lambda (a d) a)))
;
; (define (kdr c)
;   (c (lambda (a d) d)))

(define (kar c)
  (c (lambda (s a d) a)))

(define (kdr c)
  (c (lambda (s a d) d)))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define (bons kar)
  (let ([kdr '()])
    (lambda (selector)
      (selector
        (lambda (x) (set! kdr x))
        kar
        kdr))))

(define (kons a d)
  (let ([c (bons a)])
    (set-kdr c d)
    c))

(define kounter 0)
(define set-kounter 0)

(define konsC
  (let ([N 0])
    (set! kounter (lambda () N))
    (set! set-kounter (lambda (n) (set! N n)))
    (lambda (a b)
      (set! N (add1 N))
      (kons a b))))

(define (klots m)
  (if (zero? m)
    '()
    (konsC 'egg (klots (sub1 m)))))

(define (eklist? ls1 ls2)
  (cond [(null? ls1) (null? ls2)]
        [(null? ls2) #f]
        [else
          (and (eq? (kar ls1) (kar ls2))
               (eklist? (kdr ls1) (kdr ls2)))]))

(test-case "kons & klots"
  (define dozen (klots 12))
  (check-equal? (kounter) 12))

(define (kadd-at-end l)
  (if (null? l)
    (konsC 'egg '())
    (konsC (kar l) (add-at-end (kdr l)))))

(define (same? c1 c2)
  (let ([t1 (kdr c1)]
        [t2 (kdr c2)])
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ([v (eq? (kdr c1) (kdr c2))]) ; have to use eq? instead of =
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

; Gottfried W. Leibniz - pretty much invented formal logic.
; http://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz#Formal_logic

(test-case "bakers dozen"
  (define dozen (klots 12))
  (define bakers-dozen (kadd-at-end dozen))
  (define bakers-dozen-too (kadd-at-end dozen))
  (check-true (eklist? bakers-dozen bakers-dozen-too)))

; Guy L. Steele Jr.
; Growing a Language
; https://www.youtube.com/watch?v=_ahvzDzKdB0

; Gerald J. Susseman
; The Role of Programming
; https://www.youtube.com/watch?v=arMH5GjBwUQ

; Guy & Gerald are the original little schemers.

(test-case "sameness"
  (define dozen (klots 12))
  (define bakers-dozen (kadd-at-end dozen))
  (define bakers-dozen-too (kadd-at-end dozen))
  (check-true (same? bakers-dozen bakers-dozen-too))
  (check-true (same? (kons 'egg '())
                     (kons 'egg '())))) ; but my same was true?

(define (last-kons ls)
  (if (null? (kdr ls))
    ls
    (last-kons (kdr ls))))

(define (klenkth l)
  (if (null? l)
    0
    (add1 (klenkth (kdr l)))))

(define long (klots 12))

(test-case "long"
  (check-equal? (klenkth long) 12)
  (check-equal? (set-kdr (last-kons long) long) long)
  (check-equal? (klenkth long) 1) ; I got one. Hmm...
  (set-kdr (last-kons long) (kdr (kdr long)))) ; fails :(

