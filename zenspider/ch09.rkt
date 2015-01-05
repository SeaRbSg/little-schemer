#lang racket/base

(require "lib/shared.rkt")

(module+ test
  (require rackunit))

(define length
  (lambda (l)
    (cond [(null? l) 0]
          [else (add1 (length (cdr l)))])))

(module+ test
  (check-equal? (length '())
                0)
  (check-equal? (length '(a))
                1)
  (check-equal? (length '(a b))
                2))

(define eternity
  (lambda (x) (eternity x)))

(module+ test
  (define test-length
    (lambda (name f0 f1 f2)
      (test-case name
        (check-equal? (f0 '())    0)

        (check-equal? (f1 '())    0)
        (check-equal? (f1 '(a))   1)

        (check-equal? (f2 '())    0)
        (check-equal? (f2 '(a))   1)
        (check-equal? (f2 '(a b)) 2)))))

(define length0
  (lambda (l)
    (cond [(null? l) 0]
          [else (add1 (eternity (cdr l)))])))

(define length1
  (lambda (l)
    (cond [(null? l) 0]
          [else (add1 ((lambda (l)      ; length0
                         (cond [(null? l) 0]
                               [else (add1 (eternity (cdr l)))])) (cdr l)))])))

(define length2
  (lambda (l)
    (cond
     [(null? l) 0]
     [else
      (add1
       ((lambda (l)                     ; length0
          (cond
           [(null? l) 0]
           [else
            (add1
             ((lambda (l)
                (cond
                 [(null? l) 0]
                 [else
                  (add1 (eternity (cdr l)))])) (cdr l)))]))
        (cdr l)))])))

(module+ test
  (test-length "length" length0 length1 length2))

(define meta-length0
  ((lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))
   eternity))

;; dupe and replace eternity with itself

(define meta-length1
  ((lambda (f)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (f (cdr l)))])))
   ((lambda (g)
      (lambda (l)
        (cond [(null? l) 0]
              [else (add1 (g (cdr l)))])))
    eternity)))

;; ditto

(define meta-length2
  ((lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))
   ((lambda (length)
      (lambda (l)
        (cond [(null? l) 0]
              [else (add1 (length (cdr l)))])))
    ((lambda (length)
       (lambda (l)
         (cond [(null? l) 0]
               [else (add1 (length (cdr l)))])))
     eternity))))

(module+ test
  (test-length "meta-length" meta-length0 meta-length1 meta-length2))

;; pull out the eternity repetition:

(define meta2-length0
  ((lambda (mk-length)
     (mk-length eternity))
   (lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))))

(define meta2-length1
  ((lambda (mk-length)
     (mk-length (mk-length eternity)))
   (lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))))

(define meta2-length2
  ((lambda (mk-length)
     (mk-length (mk-length (mk-length eternity))))
   (lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))))

(module+ test
  (test-length "meta2-length" meta2-length0 meta2-length1 meta2-length2))

;; pass mk-length to itself instead of passing eternity:

(define meta3-length0
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (mk-length (cdr l)))])))))

(define meta3-length1
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 ((mk-length eternity) (cdr l)))])))))

(define meta3-length2 ; I cheat here, because I can't figger it out w/ eternity
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 ((mk-length mk-length) (cdr l)))])))))

(module+ test
  (test-length "meta3-length" meta3-length0 meta3-length1 meta3-length2)

  (check-equal? (((lambda (mk-length) (mk-length mk-length))
                  (lambda (mk-length)
                    (lambda (l)
                      (if (null? l) 0
                          (add1 ((mk-length eternity) (cdr l)))))))
                 '(apples))
                1)

  (check-equal? (((lambda (mk-length)
                    (lambda (l)
                      (if (null? l) 0
                          (add1 ((mk-length eternity) (cdr l))))))
                  (lambda (mk-length)
                    (lambda (l)
                      (if (null? l) 0
                          (add1 ((mk-length eternity) (cdr l)))))))
                 '(apples))
                1)

  (check-equal? ((lambda (l)
                   (if (null? l) 0
                       (add1 (((lambda (mk-length)
                                 (lambda (l)
                                   (if (null? l) 0
                                       (add1 ((mk-length eternity) (cdr l))))))
                               eternity)
                              (cdr l)))))
                 '(apples))
                1)

  (check-equal? (let ((l '(apples)))
                  (if (null? l) 0
                      (add1 (((lambda (mk-length)
                                (lambda (l)
                                  (if (null? l) 0
                                      (add1 ((mk-length eternity) (cdr l))))))
                              eternity)
                             (cdr l)))))
                1)

  (check-equal? (let ((l '(apples)))
                  (add1 (((lambda (mk-length)
                            (lambda (l)
                              (if (null? l) 0
                                  (add1 ((mk-length eternity) (cdr l))))))
                          eternity)
                         (cdr l))))
                1)

  (check-equal? (let ((l '(apples)))
                  (add1 (((lambda (mk-length)
                            (lambda (l)
                              (if (null? l) 0
                                  (add1 ((mk-length eternity) (cdr l))))))
                          eternity)
                         (cdr l))))
                1)

  (check-equal? (let ((l '(apples)))
                  (add1 ((lambda (l)
                           (if (null? l) 0
                               (add1 ((eternity eternity) (cdr l)))))
                         (cdr l))))
                1)

  (check-equal? (let ((l '(apples)))
                  (add1 (let ((l2 (cdr l)))
                          (if (null? l2) 0
                              (add1 ((eternity eternity) (cdr l2)))))))
                1)

  (check-equal? (let ((l '(apples)))
                  (add1 (let ((l2 (cdr l)))
                          0)))
                1)

  (check-equal? (add1 0)
                1))

(define meta4-length
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 ((mk-length mk-length) (cdr l)))])))))

(module+ test
  (test-length "meta4-length" meta4-length meta4-length meta4-length))

;; extract (mk-length mk-length) to recreate original length function:
;;
;;   (define meta5-length
;;     ((lambda (mk-length)
;;        (mk-length mk-length))
;;      (lambda (mk-length)
;;        ((lambda (length)
;;           (lambda (l)
;;             (cond [(null? l) 0]
;;                   [else (add1 (length (cdr l)))])))
;;         (mk-length mk-length)))))
;;
;; but this doesn't quite work, because it no longer returns a
;; function to recurse on. So wrap that up in an extra lambda

(define meta5-length
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond [(null? l) 0]
                [else (add1 (length (cdr l)))])))
      (lambda (x) ((mk-length mk-length) x))))))

(module+ test
  (test-length "meta5-length" meta5-length meta5-length meta5-length))

;; factor length out to the outside since it no longer depends on mk-length
(define meta6-length
  ((lambda (le)
     ((lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
        (le
         (lambda (x) ((mk-length mk-length) x))))))
   (lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))))

(module+ test
  (test-length "meta6-length" meta6-length meta6-length meta6-length))

;; raw Y combinator... a tad opaque... try to clean up
(define (Y1 outer)
  ((lambda (f) (f f))
   (lambda (f) (outer (lambda (x) ((f f) x))))))

;; extract arg to outer as apply
(define (Y2 outer)
  ((lambda (f) (f f))
   (lambda (f)
     (define (apply x) ((f f) x))
     (outer apply))))

;; extract all of innards as call
(define (Y3 outer)
  (define (call f)
    (define (apply x) ((f f) x))
    (outer apply))
  ((lambda (f) (f f)) call))

;; apply call to lambda-f
(define (Y4 outer)
  (define (call f)
    (define (apply x) ((f f) x))
    (outer apply))
  (call call))

(define meta7-length
  (Y4
   (lambda (length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (add1 (length (cdr l)))])))))

(module+ test
  (test-length "meta7-length" meta7-length meta7-length meta7-length)
  (check-equal? (meta7-length '(a b c d e f g h i j))
                10))

;;    (define (f x)           ...)
;;    (define  f  (lambda (x) ...))
;; (Y4 (lambda (f) (lambda (x) ...)))

(module+ test
  (test-case "y combinator"
    (define fact
      (lambda (n)
        (if (= n 1) 1
            (* n (fact (- n 1))))))

    (check-equal? (fact 10)
                  3628800)

    (check-equal? ((lambda (n)
                     ((lambda (fact)
                        (fact fact n))
                      (lambda (ft k)
                        (if (= k 1) 1
                            (* k (ft ft (- k 1))))))) 10)
                  3628800)

    (check-equal? ((Y4 (lambda (fact)
                         (lambda (n)
                           (if (= n 1) 1
                               (* n (fact (- n 1))))))) 10)
                  3628800)))
