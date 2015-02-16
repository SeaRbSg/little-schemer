#lang racket
(require rackunit)

;; Stolen from sentientmonkey
(define Y
  (lambda (le)
    ((lambda (f) (f f))
       (lambda (f)
         (le (lambda (x) ((f f) x)))))))

;; Chapter 3 definition
;; (define multirember 
;;   (lambda (a lat)
;;     (cond
;;      [(null? lat) '()]
;;      [(eq? (car lat) a) (multirember a (cdr lat))]
;;      [else (cons (car lat) (multirember a (cdr lat)))])))

;; Y combinator definition
;; (define multirember
;;   (lambda (a lat)
;;     ((Y (lambda (mr)
;;           (lambda (lat)
;;             (cond
;;              [(null? lat) '()]
;;              [(eq? a (car lat))
;;               (mr (cdr lat))]
;;              [else (cons (car lat)
;;                          (mr (cdr lat)))]))))
;;      lat)))

;; letrec definition
(define multirember
  (lambda (a lat)
    ((letrec 
         ((mr (lambda (lat)
                (cond 
                 [(null? lat) '()]
                 [(eq? a (car lat)) (mr (cdr lat))]
                 [else (cons (car lat) (mr (cdr lat)))]))))
       mr)
     lat)))

(test-case "multirember "
           [check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and)]
           [check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte)])


;; (define length
;;   ((lambda (le)
;;      ((lambda (f) (f f))
;;       (lambda (f)
;;         (le (lambda (x) ((f f) x))))))
;;    (lambda (length)
;;      (lambda (l)
;;        (cond
;;         [(null? l) 0]
;;         [else (add1 (length (cdr l)))])))))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))))

(test-case "length"
           [check-eq? (length '()) 0]
           [check-eq? (length '(a)) 1]
           [check-eq? (length '(a b c)) 3])
