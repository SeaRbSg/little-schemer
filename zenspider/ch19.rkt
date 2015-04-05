;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit)
  (require (only-in racket/function identity))

  (define letcc/value #f)

  (define-syntax check-letcc/value?
    (syntax-rules ()
      [(_ act exp) (begin (set! letcc/value act)
                          (check-equal? (identity letcc/value) exp))])))

(require (only-in "lib/shared.rkt" atom?))

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
  (check-letcc/value? (deep 6)               '((((((pizza)))))))
  (check-letcc/value? (deepB 6)              '((((((pizza)))))))
  (check-letcc/value? (toppings 'mozzarella) '((((((mozzarella)))))))
  (check-letcc/value? (toppings 'cake)       '((((((cake)))))))
  (check-letcc/value? (cons (toppings 'cake) '()) '((((((cake))))))))

(define (deep&co m k)
  (cond [(zero? m) (k 'pizza)]
        [else (deep&co (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (check-equal? (deep&co 0 identity) 'pizza)
  (check-equal? (deep&co 6 identity) '((((((pizza))))))))

(define (deep&coB m k)
  (cond [(zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza))]
        [else (deep&coB (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (check-equal? (deep&coB 4 identity)                    '((((pizza)))))
  (check-equal? (cons (toppings 'cake) (toppings 'cake)) '(((((cake)))) (((cake)))))
  (check-equal? (cons (toppings 'cake) (toppings 'cake)) '(((((cake)))) (((cake))))))

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
  (check-letcc/value? (start-it '((potato) (chips (chips (with))) fish)) 'potato))

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
  (check-letcc/value? (two-in-a-row*? '(((food) ()) (((food))))) #t))
