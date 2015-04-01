#lang racket
(require "lib/shared.rkt")
(require rackunit)

;;; page 143

(define kons cons)   ; i will redefine them later with set!
(define kar car)
(define kdr cdr)

(define lots
  (lambda (m)
    (if (zero? m) '()
        (kons 'egg (lots (sub1 m))))))

(define lenkth
  (lambda (l)
    (if (null? l) 0
        (add1 (lenkth (kdr l))))))

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
        (kons x y))))

(define add-at-end
  (lambda (l)
    (if (null? (kdr l))
        (konsC (kar l) (kons 'egg '()))
        (konsC (kar l) (add-at-end (kdr l))))))

(test-case "page 144"
  [check-equal? (add-at-end '(1 2 3 4)) '(1 2 3 4 egg)]
  [check-equal? (add-at-end (lots 3)) '(egg egg egg egg)])

;;; page 145

(require r5rs)
(define set-kdr set-cdr!)

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (if (null? (kdr ls))
                  (set-kdr ls (konsC 'egg '()))
                  ;(set-kdr ls '(egg))
                  (A (kdr ls))))))
      (A l)
      l)))

(test-case "page 144"
  (set-kounter 0)
  [check-equal? (add-at-end-too '(1 2 3 4)) '(1 2 3 4 egg)]
  [check-equal? (add-at-end-too (lots 3)) '(egg egg egg egg)]
  [check-equal? (kounter) 2])

;;; page 146

(set! kons ; given this definition ==> come up with kar and kdr
  (lambda (head tail)
    (lambda (selector)
      (selector head tail))))

(set! kar
  (lambda (k)
    (k (lambda (a d) a))))

(set! kdr
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

(set! set-kdr
  (lambda (b x)
    ((b (lambda (s a d) s)) x)))    ; <=== interesting !!

(set! kons
   (lambda (a d)
     (let ((b (bons a)))
       (set-kdr b d)
       b)))

(define to-list ; great idea Scott & Ryan !!
  (lambda (p)
    (if (null? p)
        '()
        (cons (kar p) (to-list (kdr p))))))

(test-case "page 146 c"
  (define hola (kons 'pepe (kons 'luis '())))
  (to-list hola)
  [check-equal? (to-list hola) '(pepe luis)]
  [check-equal? (kar hola) 'pepe]
  [check-equal? (kar (kdr hola)) 'luis]
  [check-equal? (kdr (kdr hola)) '()])

;;; page 147 & 148 -- lets redefine everything

(set-kounter 0)
(define dozen (lots 12))
(define bakers-dozen (add-at-end (lots 12)))
(define bakers-dozen-too (add-at-end-too (lots 12)))
(define bakers-dozen-again (add-at-end-too (lots 12)))

(test-case "basic egg baskets on procs"
  [check-equal? (lenkth dozen)              12]
  [check-equal? (to-list dozen) '(egg egg egg egg egg egg egg egg egg egg egg egg)]
  [check-equal? (lenkth bakers-dozen)       13]
  [check-equal? (lenkth bakers-dozen-too)   13]
  [check-equal? (lenkth bakers-dozen-again) 13])

;;; page 149

(define eklist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)]
      [(null? l2) #f]
      [else (and (eq? (kar l1) (kar l2))
                 (eklist? (kdr l1) (kdr l2)))])))

(test-case "eklist applied to procs"
  [check-true (eklist? (kons 'egg '()) (kons 'egg '()))]
  [check-true (eklist? bakers-dozen bakers-dozen-too)]
  [check-true (eklist? bakers-dozen-too bakers-dozen-again)])

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

(test-case "eklist applied to procs"
  [check-false (same? (kons 'egg '()) (kons 'egg '()))]
  [check-false (same? dozen bakers-dozen-too)]
  [check-false (same? bakers-dozen bakers-dozen-too)])

;;; page 151 & 152

(define last-kons
  (lambda (proc)
    (if (null? (kdr proc))
        proc
        (last-kons (kdr proc)))))

(define long (lots 12))

(test-case "God! the fun never ends..."
  [check-equal? (lenkth long) 12]
  [check-equal? (to-list long) '(egg egg egg egg egg egg egg egg egg egg egg egg)]
  [check-equal? (lenkth (last-kons long)) 1]
  [check-equal? (kar (last-kons long)) 'egg]
  [check-equal? (kdr (last-kons long)) '()])

(set-kdr (last-kons long) long)               ; setting the last '() to point to long (circular ref?)
;(eklist? long long)                          ; crash! is infinitly generating the list
;(lenkth long)                                ; crash!
;(set-kdr (last-kons long) (kdr (kdr long)))  ; crash! (kdr (kdr long)) ==> 10 eggs

;;; page 153

(define finite-lenkth
  (lambda (p)
    (let/cc infinite
      (letrec
          ((C (lambda (p q)
                (cond
                  [(same? p q) (infinite #f)]
                  [(null? q) 0]
                  [(null? (kdr q)) 1]
                  [else (+ (C (sl p) (qk q)) 
                           2)])))
           (qk (lambda (x) (kdr (kdr x))))    ; qk == quick
           (sl (lambda (x) (kdr x))))         ; sl == slow  || the same principle as linked list circular
        (cond
          [(null? p) 0]
          [else (add1 (C p (kdr p)))])))))

(test-case "finite-lenkth"
  (set! long (lots 12))
  (check-equal? (finite-lenkth long) 12)
  (set-kdr (last-kons long) long)
  (check-equal? (finite-lenkth long) #f)
  (define mongo (kons 'pie (kons 'a (kons 'la (kons 'mode '())))))
  [check-equal? (to-list mongo) '(pie a la mode)]
  (set-kdr (kdr (kdr (kdr mongo))) (kdr mongo))
  (check-equal? (finite-lenkth mongo) #f))
