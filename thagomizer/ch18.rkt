#lang racket
(require rackunit)
(require r5rs)

;; previous chapters
(define counter 0)
(define set-counter 0)

(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))


;; aliases
(define kons cons)
(define kar car)
(define kdr cdr)
(define konsC consC)
(define kounter counter)
(define set-kdr set-cdr!)
(define set-kounter set-counter)

;; New definitions
(define lots
  (lambda (m)
    (cond
     [(zero? m) '()]
     [else (kons 'egg
                 (lots (sub1 m)))])))

(test-case "lots"
           [check-equal? (lots 3) '(egg egg egg)]
           [check-equal? (lots 5) '(egg egg egg egg egg)]
           [check-equal? (lots 12) 
                         '(egg egg egg egg egg egg egg egg egg egg egg egg)])

(define lenkth
  (lambda (l)
    (cond 
     [(null? l) 0]
     [else (add1 (lenkth (kdr l)))])))

(test-case "lenkth"
           [check-equal? (lenkth (lots 3)) 3]
           [check-equal? (lenkth (lots 5)) 5]
           [check-equal? (lenkth (lots 15)) 15])

(define add-at-end
  (lambda (l)
    (cond
     [(null? (kdr l))
      (konsC (kar l)
             (kons 'egg '()))]
     [else (konsC (kar l)
                  (add-at-end (kdr l)))])))

(test-case "add-at-end"
           [check-equal? (add-at-end (lots 3)) '(egg egg egg egg)]
           [check-equal? (kounter) 3])

(define add-at-end-too
  (lambda (l)
    (letrec 
        ((A (lambda (ls)
              (cond
               [(null? (kdr ls))
                (set-kdr ls
                         (kons 'egg '()))]
               [else (A kdr ls)]))))
      (A l)
      l)))

(test-case "add-at-end-too"
           (set-kounter 0)
           [check-equal? (add-at-end-too (lots 3)) '(egg egg egg egg)])

