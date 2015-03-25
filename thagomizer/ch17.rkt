#lang racket
(require rackunit)

;; From previous chapters
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [(and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1)) (atom? (car l2))) #f]
     [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(define find
  (lambda (n Ns Rs)
    (letrec 
        ((F (lambda (ns rs)
            (cond
             [(null? ns) #f]
             [(eq? n (car ns)) (car rs)]
             [else (F (cdr ns) (cdr rs))]))))
       (F Ns Rs))))

;; This chapter

(define counter 0)
(define set-counter 0)

(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

;; (define deep
;;   (lambda (m)
;;     (if (zero? m)
;;         'pizza
;;         (cons (deep (sub1 m))
;;               '()))))

;; (define deepM
;;   (let ((Rs '())
;;         (Ns '()))
;;     (letrec 
;;         ((D (lambda (m)
;;               (if (zero? m)
;;                   'pizza
;;                   (cons (deepM (sub1 m))
;;                         '())))))
;;       (lambda (n)
;;         (let ((exists (find n Ns Rs)))
;;           (if (atom? exists)
;;               (let ((result (D n)))
;;                 (set! Rs (cons result Rs))
;;                 (set! Ns (cons n Ns))
;;                 result)
;;               exists))))))

;; (define deepM
;;   (let ((Rs '())
;;         (Ns '())
;;         (D (lambda (m)
;;              (if (zero? m)
;;                  'pizza
;;                  (cons (deepM (sub1 m)) '())))))
;;     (lambda (n)
;;       (let ((exists (find n Ns Rs)))
;;         (if (atom? exists)
;;             (let ((result (D n)))
;;               (set! Rs (cons result Rs))
;;               (set! Ns (cons n Ns))
;;               result)
;;             exists)))))

;; (define deepM
;;   (let ((Rs '())
;;         (Ns '()))
;;     (lambda (n)
;;       (let ((exists (find n Ns Rs)))
;;         (if (atom? exists)
;;             (let ((result (let ((m n))
;;                             (if (zero? m)
;;                                 'pizza
;;                                 (cons (deepM (sub1 m))
;;                                       '())))))
;;               (set! Rs (cons result Rs))
;;               (set! Ns (cons n Ns))
;;               result)
;;             exists)))))

;; (define deepM
;;   (let ((Rs '())
;;         (Ns '()))
;;     (lambda (n)
;;       (let ((exists (find n Ns Rs)))
;;         (if (atom? exists)
;;             (let ((result (if (zero? n)
;;                               'pizza
;;                               (cons (deepM (sub1 n))
;;                                     '()))))
;;               (set! Rs (cons result Rs))
;;               (set! Ns (cons n Ns))
;;               result)
;;             exists)))))

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       'pizza
                       (consC 
                        (deepM (sub1 n))
                        '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test-case "deepM"
           (set-counter 0)
           [check-equal? (deepM 5) '(((((pizza)))))]
           [check-equal? (counter) 5]
           [check-equal? (deepM 7) '(((((((pizza)))))))]
           [check-equal? (counter) 7]
           [check-equal? (deepM 0) 'pizza])

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m))
              '()))))

(test-case "deep"
           (set-counter 0)
           [check-equal? (deep 5) '(((((pizza)))))]
           [check-equal? (counter) 5]
           (set-counter 0)
           [check-equal? (deep 7) '(((((((pizza)))))))]
           [check-equal? (counter) 7])

;; (define supercounter
;;   (lambda (f)
;;     (letrec
;;         ((S (lambda (n)
;;               (if (zero? n)
;;                   (f n)
;;                   (let ()
;;                     (f n)
;;                     (S (sub1 n)))))))
;;       (S 1000))))

(set-counter 0)

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(test-case "supercounter"
           (set-counter 0)
           [check-equal? (supercounter deep) 500500]
           (set-counter 0)
           [check-equal? (supercounter deepM) 993]) ;; WTF?!?! should be 1000



(define rember1*C
  (lambda (a l)
    (letrec 
        ((R (lambda (l oh)
              (cond
               [(null? l) (oh 'no)]
               [(atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (cons (car l)
                          (R (cdr l) oh)))]
               [else
                (let ((new-car 
                       (let/cc oh (R (car l) oh))))
                  (if (atom? new-car)
                      (consC (car l)
                             (R (cdr l) oh))
                      (consC new-car (cdr l))))]))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(test-case "rember1*C"
           (set-counter 0)
           [check-equal? (rember1*C 'noodles '((food) more (food))) 
                         '((food) more (food))]
           [check-equal? (counter) 0])

(test-case ""
           [check-equal? (consC (consC 'food (quote ()))
                                (consC 'more 
                                       (consC (consC 'food (quote ()))
                                              (quote ()))))
                         '((food) more (food))]
           [check-equal? (counter) 5])

(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l) 
              (cond
               [(null? l) '()]
               [(atom? (car l)) 
                (if (eq? (car l) a)
                    (cdr l)
                    (consC (car l)
                           (R (cdr l))))]
               [else
                (let ((av (R (car l))))
                  (if (eqlist? (car l) av)
                      (consC (car l) (R (cdr l)))
                      (consC av (cdr l))))]))))
      (R l))))

(test-case "rember1*C2"
           (set-counter 0)
           [check-equal? (rember1*C2 'noodles '((food) more (food)))
                         '((food) more (food))]
           [check-equal? (counter) 5])
