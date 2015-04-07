#lang racket
(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define deep
  (lambda (m)
    (cond [(zero? m) 'pizza]
          [else
           (cons (deep (sub1 m)) '())])))


(define toppings '())

(define deepB
  (lambda (m)
    (cond [(zero? m)
           (let/cc jump
                   (set! toppings jump)
                   'pizza)]
          [else
           (cons (deepB (sub1 m))
                 '())])))
  
(deepB 6)

(cons
 (cons
  (cons (toppings 'mozzarella) ; (toppings 'mozzarella) forgets everything surrounding it.
        '())
  '())
 '())

(define deep&co
  (lambda (m k)
    (cond [(zero? m) (k 'pizza)]
          [else
           (deep&co (sub1 m)
                    (lambda (x)
                      (k (cons x '()))))])))

(define leave '())

(define walk
  (lambda (l)
    (cond [(null? l) '()]
          [(atom? (car l)) (leave (car l))]
          [else
           (let ()
             (walk (car l))
             (walk (cdr l)))])))

(define start-it
  (lambda (l)
    (let/cc here
            (set! leave here)
            (walk l))))

(define fill '())

(define waddle
  (lambda (l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (let ()
             (let/cc rest
                     (set! fill rest)
                     (leave (car l)))
             (waddle (cdr l)))]
          [else
           (let ()
             (waddle (car l))
             (waddle (cdr l)))])))

(define start-it2
  (lambda (l)
    (let/cc here
            (set! leave here)
            (waddle l))))
