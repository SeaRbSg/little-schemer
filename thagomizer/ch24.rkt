#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(define mem
  (lambda (x l)
    (cond 
     [(null? l) #f]
     [(eq-car? l x) l]
     [else (mem x (cdr l))])))

[check-equal? (mem 'tofu '(a b tofu d peas e)) '(tofu d peas e)]

[check-equal? (mem 'tofu '(a b peas d peas e)) #f]

[check-equal? (run* (out)
                    (== (mem 'tofu '(a b tofu d peas e)) out))
              '((tofu d peas e))]

[check-equal? (mem 'peas (mem 'tofu '(a b tofu d peas e)))
              '(peas e)]

[check-equal? (mem 'tofu (mem 'tofu '(a d tofu d tofu e)))
              '(tofu d tofu e)]

[check-equal? (mem 'tofu (cdr (mem 'tofu '(a b tofu d tofu e))))
              '(tofu e)]

(define memo
  (lambda (x l out)
    (conde
     [(nullo l) u#]
     [(eq-caro l x) (== l out)]
     [else
      (fresh (d)
             (cdro l d)
             (memo x d out))])))

[check-equal? (run 1 (out)
                   (memo 'tofu '(a b tofu d tofu e) out))
              '((tofu d tofu e))]

[check-equal? (run 1 (out)
                   (fresh (x)
                          (memo 'tofu `(a b ,x d tofu e) out)))
              '((tofu d tofu e))]

;; This is wear it gets awesome
[check-equal? (run* (r)
                   (memo r
                         '(a b tofu d tofu e)
                         '(tofu d tofu e)))
              '(tofu)]

[check-equal? (run* (q)
                    (memo 'tofu '(tofu e) '(tofu e))
                    (== #t q))
              '(#t)]

[check-equal? (run* (q)
                    (memo 'tofu '(tofu e) '(tofu))
                    (== #t q))
              '()]

[check-equal? (run* (x)
                    (memo 'tofu '(tofu e) `(,x e)))
              '(tofu)]

[check-equal? (run* (x)
                    (memo 'tofu '(tofu e) '(peas ,x)))
              '()]

[check-equal? (run* (out)
                    (fresh (x)
                           (memo 'tofu `(a b ,x d tofu e) out)))
              '((tofu d tofu e) (tofu e))]

[check-equal? (run 5 (z)
                   (fresh (u)
                          (memo 'tofu `(a b tofu d tofu e . ,z) u)))
              '(_.0                  ;; first tofu in l unifies with 'tofu
                _.0                  ;; second tofu in l unifies with 'tofu
                (tofu . _.0)         ;; when mem hits z
                (_.0 tofu . _.1)     ;; something tofu something else
                (_.0 _.1 tofu . _.2) ;; something something tofu something
                )]

(define rember
  (lambda (x l)
    (cond
     [(null? l) '()]
     [(eq-car? l x) (cdr l)]
     [else
      (cons (car l)
            (rember x (cdr l)))])))

[check-equal? (rember 'peas '(a b peas d peas e))
              '(a b d peas e)]

(define rembero
  (lambda (x l out)
    (conde
     [(nullo l) (== '() out)]
     [(eq-caro l x) (cdro l out)]
     [else
      (fresh (res)
             (fresh (d)
                    (cdro l d)
                    (rembero x d res))
             (fresh (a)
                    (caro l a)
                    (cons a res out)))])))

;; (define rembero
;;   (lambda (x l out)
;;     (conde
;;      [(nullo l) (== '() out)]
;;      [(eq-caro l x) (cdro l out)]
;;      [else
;;       (fresh (res d a)
;;              (cons a d l)
;;              (rembero x d res)
;;              (cons a res out))])))

(run 1 (out)
     (fresh (y)
            (rembero 'peas `(a b ,y d peas e) out)))
