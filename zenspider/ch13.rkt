#lang racket/base

(require rackunit)
(require "lib/shared.rkt")

(module+ test
  (require rackunit))

(define member?                         ; from ch12.scm
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                     (cond [(null? l) #f]
                           [(eq? (car l) a) #t]
                           [else (yes? (cdr l))]))))
      (yes? lat))))

(module+ test
  (test-case "member?"
    (check-false (member? 42 '(a b c)))
    (check-true (member? 'b '(a b c)))))

(define intersect1
  (lambda (set1 set2)
    (cond [(null? set1) '()]
          [(member? (car set1) set2)
           (cons (car set1)
                 (intersect1 (cdr set1) set2))]
          [else  (intersect1 (cdr set1) set2)])))

(define intersect2
  (lambda (set1 set2)
    (letrec ((I (lambda (set)
                  (cond [(null? set) '()]
                        [(member? (car set) set2)
                         (cons (car set)
                               (I (cdr set)))]
                        [else  (I (cdr set))]))))
      (I set1))))

(define intersectall1
  (lambda (lset)
    (letrec ((I (lambda (lset)
                  (cond [(null? (cdr lset)) (car lset)]
                        [else (intersect2 (car lset) (I (cdr lset)))]))))
      (cond ((null? lset) '())
            (else (I lset))))))

(define intersectall2
  (lambda (lset)
    (let/cc hop
            (letrec
                ((I (lambda (lset)
                      (cond [(null? (car lset)) (hop '())]
                            [(null? (cdr lset)) (car lset)]
                            [else (intersect2 (car lset) (I (cdr lset)))]))))
              (cond ((null? lset) '())
                    (else (I lset)))))))

(module+ test
  (check-equal? (intersectall2 '((a b c) (b c) (b)))
                '(b)))

(define intersect3
  (lambda (set1 set2)
    (letrec ((I (lambda (set)
                  (cond [(null? set) '()]
                        [(member? (car set) set2)
                         (cons (car set)
                               (I (cdr set)))]
                        [else  (I (cdr set))]))))
      (cond
       ((null? set2) '())
       (else (I set1))))))

(define intersectall3
  (lambda (lset)
    (let/cc hop
            (letrec
                ((A (lambda (lset)
                      (cond [(null? (car lset)) (hop '())]
                            [(null? (cdr lset)) (car lset)]
                            [else (intersect3 (car lset) (A (cdr lset)))])))
                 (I (lambda (s1 s2)
                      (letrec ((J (lambda (s1)
                                    (cond [(null? s1) '()]
                                          [(member? (car s1) s2) (J (cdr s1))]
                                          [else (cons (car s1)
                                                      (J (cdr s1)))]))))
                        (cond [(null? s2) (hop '())]
                              [else (J s1)])))))
              (cond [(null? lset) '()]
                    [else (A lset)])))))

(module+ test
  (check-equal? (intersectall3 '((a b c) (b c) (b)))
                '(b)))

(define rember
  (lambda (a lat)
    (letrec ((R (lambda (lat)
                  (cond [(null? lat) '()]
                        [(equal? (car lat) a) (cdr lat)]
                        [else (cons (car lat)
                                    (R (cdr lat)))]))))
      (R lat))))

(module+ test
  (check-equal? (rember 'b '(a b c b d))
                '(a c b d)))

(define rember-beyond-first
  (lambda (a lat)
    (letrec ((R (lambda (lat)
                  (cond [(null? lat) '()]
                        [(eq? (car lat) a) '()]
                        [else (cons (car lat)
                                    (R (cdr lat)))]))))
      (R lat))))

(module+ test
  (check-equal? (rember-beyond-first 'd '(a b c d e f g))
                '(a b c)))

(define rember-upto-last1
  (lambda (a orig-lat)
    (letrec ((R (lambda (lat)
                  (cond [(null? lat) orig-lat]
                        [(eq? (car lat) a) (cdr lat)]
                        [else (R (cdr lat))]))))
      (R orig-lat))))

(module+ test
  (check-equal? (rember-upto-last1 'd '(a b c d e f g))
                '(e f g))
  (check-equal? (rember-upto-last1 'z '(a b c d e f g))
                '(a b c d e f g)))

(define rember-upto-last2
  (lambda (a lat)
    (let/cc
     skip
     (letrec ((R (lambda (lat)
                   (cond [(null? lat) '()]
                         [(eq? (car lat) a) (skip (R (cdr lat)))]
                         [else (cons (car lat)
                                     (R (cdr lat)))]))))
       (R lat)))))

(module+ test
  (check-equal? (rember-upto-last2 'd '(a b c d e f g))
                '(e f g))
  (check-equal? (rember-upto-last2 'z '(a b c d e f g))
                '(a b c d e f g)))
