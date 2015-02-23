#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a 
                                      (cdr l))))))))

(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a lat)
            (cond
             ((null? lat) (quote ()))
             ((test? (car lat) a)
              (m-f a (cdr lat)))
             (else
              (cons (car lat)
                    (m-f a (cdr lat))))))))
      m-f)))

(define multirember
  (letrec
      ((mr (lambda (a lat)
             (cond
               ((null? lat) (quote ()))
               ((eq? (car lat) a)
                (mr a (cdr lat)))
               (else
                (cons (car lat)
                      (mr a (cdr lat))))))))
    mr))

(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
      (yes? lat))))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2)
                 (U (cdr set)))
                (else (cons (car set)
                            (U (cdr set)))))))
         (M?
          (lambda (a lat)
            (letrec
                ((N? (lambda (lat)
                       (cond
                         ((null? lat) #f)
                         ((eq? (car lat) a) #t)
                         (else (N? (cdr lat)))))))
              (N? lat)))
      (U set1))))

(define two-in-a-row?
    (letrec
        ((W (lambda (a lat)
              (cond
                ((null? lat) #f)
                (else (or (eq? (car late) a)
                          (W (car lat)
                             (cdr lat))))))))
      (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))
    
(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
                ((null? tup) (quote ()))
                (else
                 (cons (+ sss (car tup))
                       (S (+ sss (car tup))
                          (cdr tup))))))))
      (S 0 tup))))
    
(define scramble
  (lambda (tup)
    (letrec
        ((P
          (lambda (tup rp)
            (cond
              ((null? tup) (quote ()))
              (else (cons (pick (car tup)
                                (cons (car tup) rp))
                          (P (cdr tup)
                             (cons (car tup) rp))))))))
      (P tup (quote ())))))

(define pick
  (lambda (n lat)
    (cond
      [(eq? n '1) (car lat)]
      [else (pick (- n 1) (cdr lat))])))

