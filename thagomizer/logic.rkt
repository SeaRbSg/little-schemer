#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(define memberrevo
  (lambda (x l)
    (conde 
     [s# (fresh (d)
                (cdro l d)
                (memberrevo x d))]
     [else (eq-caro l x)])))

(define onceo
  (lambda (g)
    (condu
     [g s#]
     [else u#])))

(define membero
  (lambda (x l)
    (conde 
     [(eq-caro l x) s#]
     [(nullo '())
      (fresh (d)
             (cdro l d)
             (membero x d))])))


(define distincto 
  (lambda (l)
    (conde 
     [(nullo l) s#]
     [else
      (fresh (head tail)
             (conso head tail l)
             (conda
              [(membero head tail) u#]
              [(distincto tail) s#]))])))

(test-case "distincto"
           [check-equal? (run* (r)
                               (distincto '())
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a b c))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a a))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (distincto '(a b a))
                               (== #t r))
                         '()]
           )


;; Something is a permutation of l if:
;;  the car of l is in p somewhere and
;;  the rest of l is in p with car of l removed
;; (define permuteo
;;   (lambda (l p)
;;     (conde
;;      [(nullo l) (nullo p)]
;;      [else
;;       (fresh (l_head l_tail new_p)
;;              (conso l_head l_tail l)
;;              (conde 
;;               [(membero l_head p)])
            
;; [check-equal? (run* (r)
;;                     (fresh (x)
;;                            (possibilities x 'a 'b)
;;                            (== x r)))
;;               '((a a) (a b) (b a) (b b))]



(define logic
  (lambda (l)
    (fresh (v w x y z)
           (all 
            (membero 'A `(,w ,x ,y ,z)) ;; Adam does not live on the top
            (membero 'B `(,v ,w ,x ,y)) ;; Bill does not live on the bottom
            (membero 'C `(,w ,x ,y))    ;; Cora lives in the middle
            (conde
             [(== 'D v) (membero 'B `(,w ,x ,y))]
             [(== 'D w) (membero 'B `(,x ,y))]
             [(== 'D x) (== 'B y)])
            (conde 
             [(== 'C w) (== 'E y)]
             [(== 'C w) (== 'E z)]
             [(== 'C x) (== 'E v)]
             [(== 'C x) (== 'E z)]
             [(== 'C y) (== 'E v)]
             [(== 'C y) (== 'E w)])
            (conde
             [(== 'C w) (== 'B y)]
             [(== 'C w) (== 'B z)]
             [(== 'C x) (== 'B v)]
             [(== 'C x) (== 'B z)]
             [(== 'C y) (== 'B v)]
             [(== 'C y) (== 'B w)])
            )
           (== l `(,v ,w ,x ,y ,z)))))

[check-equal? (run* (r)
                    (logic r))
              '((D C A B E))]
             
             

;; 1. Adam does not live on the top floor.
;; 2. Bill does not live on the bottom floor.
;; 3. Cora does not live on either the top or the bottom floor.
;; 4. Dale lives on a higher floor than does Bill.
;; 5. Erin does not live on a floor adjacent to Cora's.
;; 6. Cora does not live on a floor adjacent to Bill's.
