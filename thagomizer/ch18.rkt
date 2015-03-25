#lang racket
(require rackunit)

;; New definitions

;; bons takes an arg, sets the cdr to empty, and returns another function 
;; the returned function takes an arg that decides whether to call
;; the setter, or return the car or cdr

(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(define kounter 0)
(define set-kounter 0)

(define konsC
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-kounter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

(test-case "exploration"
           ;; create a new "cons"
           (define example (bons 'a))
           ;; verify that the first is a
           [check-equal? (example (lambda (setter first rest) first)) 'a]
           ;; verify that kar works
           [check-equal? (kar example) 'a]
           ;; extract the setter from the "cons"
           (define setter (example (lambda (setter first rest) setter)))
           ;; use the setter to set the kdr
           (setter 'b)
           ;; verify that the rest is 'b 
           [check-equal? (example (lambda (setter first rest) rest)) 'b]
           ;; verify that the kdr is 'b
           [check-equal? (kdr example) 'b]
           ;; set-kdr using the function
           (set-kdr example 'c)
           ;; verify
           [check-equal? (kdr example) 'c])

(define lots
  (lambda (m)
    (cond
     [(zero? m) '()]
     [else (konsC 'egg
                 (lots (sub1 m)))])))

(define lenkth
  (lambda (l)
    (cond 
     [(null? l) 0]
     [else (add1 (lenkth (kdr l)))])))

(test-case "lenkth & lots"
           (set-kounter 0)
           [check-equal? (lenkth (lots 3)) 3]
           [check-equal? (kar (lots 3)) 'egg]
           [check-equal? (lenkth (lots 5)) 5]
           [check-equal? (lenkth (lots 15)) 15])

(define add-at-end
  (lambda (l)
    (cond
     [(null? (kdr l))
      (konsC (kar l)
             (konsC 'egg '()))]
     [else (konsC (kar l)
                  (add-at-end (kdr l)))])))

(test-case "add-at-end"
           (define example (lots 3))
           (set-kounter 0)
           (set! example (add-at-end example))
           [check-equal? (lenkth example) 4]
           [check-equal? (kounter) 4])  ;; one for the base case

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
               [(null? (kdr ls))
                (set-kdr ls
                         (konsC (quote egg) (quote ())))]
               [else (A (kdr ls))]))))
      (A l)
      l)))
               
(test-case "add-at-end-too"
           (define example (lots 3))
           (set-kounter 0)
           (set! example (add-at-end-too example))
           [check-equal? (lenkth example) 4]
           [check-equal? (kounter) 1])

(define dozen 0)
(define bakers-dozen 0)
(define bakers-dozen-too 0)
(define bakers-dozen-again 0)

(test-case "no name"
           (set-kounter 0)
           (set! dozen (lots 12))
           [check-equal? (kounter) 12]
           (set! bakers-dozen (add-at-end dozen))
           [check-equal? (kounter) 25] 
           (set! bakers-dozen-too (add-at-end-too dozen))
           [check-equal? (kounter) 26] 
           (set! bakers-dozen-again (add-at-end dozen))
           [check-equal? (kounter) 40])

(define eklist?
  (lambda (ls1 ls2)
    (cond
     [(null? ls1) (null? ls2)]
     [(null? ls2) #f]
     [else 
      (and (eq? (kar ls1) (kar ls2))
           (eklist? (kdr ls1) (kdr ls2)))])))

(test-case "eklist?"
           [check-true (eklist? bakers-dozen bakers-dozen-too)])

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

(test-case "same?"
           [check-false (same? bakers-dozen bakers-dozen-too)]
           [check-false (same? (kons 'egg '()) (kons 'egg '()))])

(define last-kons
  (lambda (ls)
    (cond
     [(null? (kdr ls)) ls]
     [else (last-kons (kdr ls))])))

(define long (lots 12))

(set-kdr (last-kons long) long)

(define finite-lenkth
  (lambda (p)
    (let/cc infinite
            (letrec 
                ((C (lambda (p q)
                      (cond
                       [(same? p q)
                        (infinite #f)]
                       [(null? q) 0]
                       [(null? (kdr q)) 1]
                       [else 
                        (+ (C (sl p) (qk q)) 2)])))
                 (qk (lambda (x) (kdr (kdr x))))
                 (sl (lambda (x) (kdr x))))
              (cond
               [(null? p) 0]
               [else (add1 (C p (kdr p)))])))))

(finite-lenkth long)
