;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit))
(require "lib/shared.rkt")

(define (I x) x) ; just for testing

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else (cons (deep (sub1 m)) '())]))

(define toppings #f)
(define (deepB m)
  (cond [(zero? m)
         (let/cc jump
                 (set! toppings jump)
                 'pizza)]
        [else (cons (deepB (sub1 m)) '())]))

(module+ test
  (check-equal? (deep 6)
                '((((((pizza))))))))

(module+ test
  (define x (deepB 6))

  (check-equal? (I x)
                '((((((pizza))))))))

(module+ test
  (set! x (toppings 'mozzarella))

  (check-equal? (I x)
                '((((((mozzarella))))))))

(module+ test
  (set! x (toppings 'cake))

  (check-equal? (I x)
                '((((((cake))))))))

(module+ test
  (set! x (cons (toppings 'cake) '()))

  (check-equal? (I x)
                '((((((cake))))))))

(define (deep&co m k)
  (cond [(zero? m) (k 'pizza)]
        [else (deep&co (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (check-equal? (deep&co 0 I)
                'pizza)
  (check-equal? (deep&co 6 I)
                '((((((pizza))))))))

(define (deep&coB m k)
  (cond [(zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza))]
        [else (deep&coB (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (check-equal? (deep&coB 4 I)
                '((((pizza))))))

(module+ test
  (set! x (cons (toppings 'cake)
                (toppings 'cake)))

  (check-equal? (I x)
                '(((((cake)))) (((cake))))))

(module+ test
  (set! x (cons (toppings 'cake)
                (toppings 'cake)))

  (check-equal? (I x)
                '(((((cake)))) (((cake))))))

(define leave #f)
(define (walk l)
  (cond [(null? l) '()]
        [(atom? (car l)) (leave (car l))]
        [else (walk (car l))
              (walk (cdr l))]))

(define (start-it l)
  (let/cc here
          (set! leave here)
          (walk l)))

(module+ test
  (set! x (start-it '((potato) (chips (chips (with))) fish)))

  (check-equal? (I x)
                'potato))

(define two-in-a-row*?
  (letrec ((T?
            (lambda (a)
              (let ((n (get-next 0)))
                (if (atom? n)
                    (or (eq? n a) (T? n))
                    #f))))
           (get-next
            (lambda (x)
              (let/cc here-again
                      (set! leave here-again)
                      (fill 'go))))
           (fill
            (lambda (x) x))
           (waddle
            (lambda (l)
              (cond [(null? l) '()]
                    [(atom? (car l))
                     (let/cc rest
                             (set! fill rest)
                             (leave (car l)))
                     (waddle (cdr l))]
                    [else
                     (waddle (car l))
                     (waddle (cdr l))])))
           (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                         (set! leave here)
                         (waddle l)
                         (leave '()))))
        (if (atom? fst) (T? fst) #f)))))

(module+ test
  (set! x (two-in-a-row*? '(((food) ()) (((food))))))

  (check-true (I x)))
