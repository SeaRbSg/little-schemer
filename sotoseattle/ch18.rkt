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

; Detour to understand bons...

(define tracking_fdr 'none)
(define tracking_tot 0)

(define fons                           ; semantically derived from the latin 'f@#&ed up cons'
  (lambda (far)                        ; we pass one arg
    (let ((fdr 0))                     ; a closure where fdr is initialized to 0
      (lambda (selector)               ; returns a function F1 that takes a single arg (another function F2)
        (set! tracking_tot (selector   ; all F1 does is call F2 with 3 arguments x1 x2 x3
         (lambda (x) (set! fdr x))     ; x1, call like (x1 something), and it will set fdr to something (see test cawse)
         far                           ; x2, the far that we have from the outer closure
         fdr))                         ; x3, the fdr initialized and now set! to something (x1)
        (set! tracking_fdr fdr)))))    ; after calling F2, we return fdr (to check it works)

(define pp
  (lambda (x1 x2 x3)
    (x1 20)
    (+ x2 x3)))

(test-case "page 146 b"
  ((fons 7) pp)
  [check-equal? tracking_fdr 20]
  [check-equal? tracking_tot 7])       ; beware!!!! when pp adds x2 and x3, x3 is still 0!!!
                                       ; only after and outside of the selector, the x3/fdr is reset!!!

; ... and now we resume our regularly scheduled programming 

(define bons                           ; semantically derived from the latin 'bullshit cons'
  (lambda (bar)
    (let ((bdr '()))
      (lambda (selector)               ; s
        (selector 
         (lambda (x) (set! bdr x))     
         bar                           ; a
         bdr)))))                      ; d

(set! kar
  (lambda (b)
    (b (lambda (s a d) a))))

(set! kdr
  (lambda (b)
     (b (lambda (s a d) d))))

(define set-kdr
  (lambda (b x)
    ((b (lambda (s a d) s)) x)))

(set! kons
   (lambda (a d)
     (let ((b (bons a)))
       (set-kdr b d)
       b)))

(test-case "page 146 c"
  (define hola (kons 'pepe '(luis)))
  [check-equal? (kar hola) 'pepe]
  [check-equal? (kdr hola) '(luis)])

;;; page 147 & 148 -- lets redefine everything

(set! lots
  (lambda (m)
    (if (zero? m) '()
        (kons 'egg (lots (sub1 m))))))

(set! add-at-end
  (lambda (proc)
    (if (null? (kdr proc))
        (kons (kar proc) (kons 'egg '()))
        (kons (kar proc) (add-at-end (kdr proc))))))

(set! add-at-end-too
  (lambda (proc)
    (letrec
        ((A (lambda (p)
              (if (null? (kdr p))
                  (set-kdr p (kons 'egg '()))
                  (A (kdr p))))))
      (A proc)
      proc)))

(set-kounter 0)
(define dozen (lots 12))
(define bakers-dozen (add-at-end dozen))
(define bakers-dozen-too (add-at-end-too dozen))
(define bakers-dozen-again (add-at-end-too dozen))

;;; page 149

(define eklist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)]
      [(null? l2) #f]
      [else (and (eq? (car l1) (car l2))
                 (eklist? (cdr l1) (cdr l2)))])))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(kar dozen)
(kdr dozen)
(kdr bakers-dozen-too)
(same? dozen bakers-dozen)
;(kdr bakers-dozen-too)
;(same? bakers-dozen bakers-dozen-too)

;; Oh dammit!