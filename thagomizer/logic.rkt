#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")


[check-equal? (run* (r)
                    (nullo '(a b c))
                    (== #t r))
              '()]

[check-equal? (run* (r)
                    (nullo '())
                    (== #t r))
              '(#t)]

[check-equal? (run* (r)
                    (caro '(a b c) 'a)
                    (== #t r))
              '(#t)]

[check-equal? (run* (r)
                    (caro '(a b c) 'b)
                    (== #t r))
              '()]

[check-equal? (run* (r) 
                    (fresh (x)
                           (cdro '(a b c) x)
                           (caro x 'b))
                    (== #t r))
              '(#t)]

(define includeo
  (lambda (i l)
    (conde
     [(nullo l) u#]     
     [(caro l i) s#]
     [else 
      (fresh (new_l)
             (cdro l new_l)
             (includeo i new_l))])))


(test-case "includeo"
           [check-equal? (run* (r) 
                               (includeo 'a '(a b c))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r) 
                               (includeo 'b '(a b c))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r) 
                               (includeo 'c '(a b c))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r) 
                               (includeo 'd '(a b c))
                               (== #t r))
                         '()])
           



;; (define logic
;;   (lambda (a b c d e)))



;; 1. Adam does not live on the top floor.
;; 2. Bill does not live on the bottom floor.
;; 3. Cora does not live on either the top or the bottom floor.
;; 4. Dale lives on a higher floor than does Bill.
;; 5. Erin does not live on a floor adjacent to Cora's.
;; 6. Cora does not live on a floor adjacent to Bill's.
