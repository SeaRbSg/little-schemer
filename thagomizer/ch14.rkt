#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))

(define try
  (lambda (x alpha beta)
    (let/cc success
            (let/cc x
                    (success alpha))
            beta)))

;; Previous chapters
(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [(and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1)) (atom? (car l2))) #f]
     [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

;; (define leftmost
;;   (lambda (l)
;;     (cond
;;      [(atom? (car l)) (car l)]
;;      [else (leftmost (car l))])))

;; (define leftmost
;;   (lambda (l)
;;     (cond
;;      [(null? l) l]
;;      [(atom? (car l)) (car l)]
;;      [(atom? (leftmost (car l))) (leftmost (car l))]
;;      [else (leftmost (cdr l))])))

;; (define leftmost 
;;   (lambda (l)
;;     (cond
;;      [(null? l) '()]
;;      [(atom? (car l)) (car l)]
;;      [else (let ((a (leftmost (car l))))
;;              (cond
;;               [(atom? a) a]
;;               [else (leftmost (cdr l))]))])))

(define leftmost
  (lambda (l)
    (let/cc skip
            (letrec ((lm (lambda (l)
                           (cond 
                            [(null? l) '()]
                            [(atom? (car l)) (skip (car l))]
                            [else (let ()
                                    (lm (car l))
                                    (lm (cdr l)))]))))
              (lm l)))))

(test-case "leftmost"
           [check-equal? (leftmost '(((a) b) (c d))) 'a]
           [check-equal? (leftmost '(((a) ()) () (e))) 'a]
           [check-equal? (leftmost '(((() a) ()))) 'a]
           [check-equal? (leftmost '()) '()]
           [check-equal? (leftmost '(((a)) b (c))) 'a])


;; (define rember1*
;;   (lambda (a l)
;;     (cond
;;      [(null? l) l]
;;      [(atom? (car l))
;;       (cond
;;        [(eq? (car l) a) (cdr l)]
;;        [else (cons (car l) (rember1* a (cdr l)))])]
;;      [else 
;;       (cond
;;        [(eqlist? (rember1* a (car l)) (car l))
;;         (cons (car l) (rember1* a (cdr l)))]
;;        [else (cons (rember1* a (car l)) (cdr l))])])))

;; (define rember1*
;;   (lambda (a l)
;;     (letrec 
;;         ((R (lambda (l)
;;                   (cond 
;;                    [(null? l) l]
;;                    [(atom? (car l))
;;                     (cond 
;;                      [(eq? (car l) a) (cdr l)]
;;                      [else (cons (car l) (R (cdr l)))])]
;;                    [else 
;;                     (cond
;;                      [(eqlist? (R (car l)) (car l))
;;                       (cons (car l) (R (cdr l)))]
;;                      [else (cons (R (car l)) (cdr l))])]))))
;;       (R l))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               [(null? l) '()]
               [(atom? (car l)) 
                (cond
                 [(eq? (car l) a) (cdr l)]
                 [else (cons (car l) (R (cdr l)))])]
               [else 
                (let ((l-without-a (R (car l))))
                  (cond
                   [(eqlist? l-without-a (car l))
                    (cons (car l) (R (cdr l)))]
                   [else (cons l-without-a (cdr l))]))]))))
      (R l))))

(test-case "rember1*"
           [check-equal? (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad)) '((Swedish rye) (French (mustard turkey)) salad)]
           [check-equal? (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes)) '((pasta) pasta (noodles meat sauce) meat tomatoes)])


(define max
  (lambda (n m)
    (if (> n m) n m)))

;; (define depth*
;;   (lambda (l)
;;     (cond
;;      [(null? l) 1]
;;      [(atom? (car l)) (depth* (cdr l))]
;;      [else
;;       (cond
;;        [(> (depth* (cdr l)) (add1 (depth* (car l))))
;;         (depth* (cdr l))]
;;        [else
;;         (add1 (depth* (car l)))])])))

;; (define depth*
;;   (lambda (l)
;;     (cond
;;      [(null? l) 1]
;;      [(atom? (car l)) (depth* (cdr l))]
;;      [else
;;       (let ((depth-car (add1 (depth* (car l)))) (depth-cdr (depth* (cdr l))))
;;         (cond
;;          [(> depth-cdr depth-car) depth-cdr]
;;          [else depth-car]))])))

;; BLECH
;; (define depth*
;;   (lambda (l)
;;     (cond
;;      [(null? l) 1]
;;      [else
;;       (let ((depth-cdr (depth* (cdr l))))
;;         (cond
;;          [(atom? (car l)) depth-cdr]
;;          [else 
;;           (let ((depth-car (add1 (depth* (car l)))))
;;             (cond
;;              [(> depth-cdr depth-car) depth-cdr]
;;              [else depth-car]))]))])))

(define depth*
  (lambda (l)
    (cond
     [(null? l) 1]
     [(atom? (car l)) (depth* (cdr l))]
     [else
      (let ((depth-car (add1 (depth* (car l)))) (depth-cdr (depth* (cdr l))))
        (max depth-cdr depth-car))])))


(test-case "depth*"
           [check-eq? (depth* '((pickled) peppers (peppers pickled))) 2]
           [check-eq? (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4]
           [check-eq? (depth* '(c (b (a b) a) a)) 3])


(define scramble 
  (lambda (tup)
    (letrec
        [(pick (lambda (n lat)
                 (cond
                  [(eq? n 1) (car lat)]
                  [else (pick (sub1 n) (cdr lat))])))
         (P (lambda (tup rev-pre)
              (cond
               [(null? tup) '()]
               [else 
                (let ((consd-tup (cons (car tup) rev-pre)))
                  (cons (pick (car tup) consd-tup)
                        (P (cdr tup) consd-tup)))])))]
      (P tup '()))))

(test-case "scramble"
           [check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9)]
           [check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1)]
           [check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)])


;; (define rm
;;   (lambda (a l oh)
;;     (cond
;;      [(null? l) (oh 'no)]
;;      [(atom? (car l))
;;       (if (eq? (car l) a)
;;           (cdr l)
;;           (cons (car l) (rm a (cdr l) oh)))]
;;      [else 
;;       (if (atom? 
;;            (let/cc oh
;;                   (rm a (car l) oh)))
;;           (cons (car l)
;;                 (rm a (cdr l) oh))
;;           (cons (rm a (car l) 0)
;;                 (cdr l)))])))

(define rm
  (lambda (a l oh)
    (cond
     [(null? l) (oh 'no)]
     [(atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm a (cdr l) oh)))]
     [else
      (let ((new-car 
             (let/cc oh
                     (rm a (car l) oh))))
        (if (atom? new-car)
            (cons (car l)
                  (rm a (cdr l) oh))
            (cons new-car (cdr l))))])))
