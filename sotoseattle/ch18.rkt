#lang racket
(require "lib/shared.rkt")
(require rackunit)

;;; page 143

(define lots
  (lambda (m)
    (if (zero? m) '()
        (cons 'egg (lots (sub1 m))))))

(define lenkth
  (lambda (l)
    (if (null? l) 0
        (add1 (lenkth (cdr l))))))

(test-case "page 143"
  [check-equal? (lots 3)  '(egg egg egg)]
  [check-equal? (lenkth (lots 3))  3]
  [check-equal? (lots 5)  '(egg egg egg egg egg)]
  [check-equal? (lenkth (lots 5))  5]
  [check-equal? (lots 12) '(egg egg egg egg egg egg egg egg egg egg egg egg)]
  [check-equal? (lenkth (lots 12))  12])

;;; page 144

(define kounter '())
(define set-kounter '())
(define konsC
    (let ((N 0))
      (set! kounter (lambda () N))
      (set! set-kounter (lambda (x) (set! N x)))
      (lambda (x y)
        (set! N (add1 N))
        (cons x y))))

(define add-at-end
  (lambda (l)
    (if (null? (cdr l))
        (konsC (car l) '(egg))
        (konsC (car l) (add-at-end (cdr l))))))

(test-case "page 144"
  [check-equal? (add-at-end '(1 2 3 4)) '(1 2 3 4 egg)]
  [check-equal? (add-at-end (lots 3)) '(egg egg egg egg)])

;;; page 145

(require r5rs)

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (if (null? (cdr ls))
                  (set-cdr! ls (konsC 'egg '()))
                  ;(set-cdr! ls '(egg))
                  (A (cdr ls))))))
      (A l)
      l)))

(test-case "page 144"
  (set-kounter 0)
  [check-equal? (add-at-end-too '(1 2 3 4)) '(1 2 3 4 egg)]
  [check-equal? (add-at-end-too (lots 3)) '(egg egg egg egg)]
  [check-equal? (kounter) 2])

;;; page 146

(define kons ; given this definition ==> come up with kar and kdr
  (lambda (head tail)
    (lambda (selector)
      (selector head tail))))

(define kar
  (lambda (k)
    (k (lambda (a d) a))))

(define kdr
  (lambda (k)
    (k (lambda (a d) d))))

(test-case "page 146 a"
  [check-equal? ((kons 1 2) +) 3]             ; Oh, smashing, groovy, yay !!
  [check-equal? ((kons 'q '()) cons) '(q)]    ; Ooo, Behave!
  [check-equal? (kar (kons 'q '())) 'q]       ; Its time to swing, baby!
  [check-equal? (kdr (kons 'q '())) '()])     ; Are you kidding, baby? I put the "grrrr" in swinger, baby! Yeah!

; Relax, Javier, don't have a thrombo.

(define bons ; semantically derived from the latin 'bullshit cons'
  (lambda (bar)
    (let ((bdr '()))
      (lambda (selector)
        (selector 
         (lambda (x) (set! bdr x))     ; s
         bar                           ; a
         bdr)))))                      ; d

(define bar
  (lambda (b)
    (b (lambda (s a d) a))))

(define bdr
  (lambda (b)
     (b (lambda (s a d) d))))
