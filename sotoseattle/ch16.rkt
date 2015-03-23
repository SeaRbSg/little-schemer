#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require test-engine/racket-tests)
;(require racket/trace)

(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)

(module+ test
  (define x last) ; setting x to whatever was in last for comparisson purposes
  (check-equal? (sweet-tooth 'chocolate) '(chocolate cake))
  (check-equal? last x)
  (check-equal? (sweet-tooth 'fruit) '(fruit cake)))

(define sweet-tooth-L
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(module+ test
  (check-equal? (sweet-tooth-L 'chocolate) '(chocolate cake))
  (check-equal? last 'chocolate)
  (check-equal? (sweet-tooth-L 'fruit) '(fruit cake))
  (check-equal? last 'fruit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ingredients '())

(define sweet-tooth-R
  (lambda (food)
    (set! last food)
    (set! ingredients (cons last ingredients))
    (cons food (cons 'cake '()))))

(module+ test
  (check-equal? (sweet-tooth-R 'chocolate) '(chocolate cake))
  (check-equal? last 'chocolate)
  (check-equal? (sweet-tooth-R 'stinky_tofu) '(stinky_tofu cake))
  (check-equal? last 'stinky_tofu)
  (check-equal? (sweet-tooth-R 'garlic) '(garlic cake))
  (check-equal? last 'garlic)
  (check-equal? ingredients '(garlic stinky_tofu chocolate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define deep
  (lambda (n)
    (cond 
      [(zero? n) 'pizza]
      [else (cons (deep (- n 1)) '())])))

(module+ test
  (check-equal? (deep 3) '(((pizza))))
  (check-equal? (deep 0) 'pizza))

(define Ns_1 '())

(define deep-R  ; remember all the input arguments n
  (lambda (n)
    (set! Ns_1 (cons n Ns_1))
    (deep n)))

(module+ test
  (check-equal? (deep-R 3) '(((pizza))))
  (check-equal? Ns_1 '(3)))

(define Ns '())
(define Rs '())

(define deep-R2
  (lambda (n)
    (let ((output (deep n)))
    (set! Ns (cons n Ns))
    (set! Rs (cons output Rs))
    output)))

(module+ test
  (check-equal? (deep-R2 0) 'pizza)
  (check-equal? (deep-R2 2) '((pizza)))
  (check-equal? Ns '(2 0))
  (check-equal? Rs '(((pizza)) pizza)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define find-v1
  (lambda (n Ns Rs)
    (cond
      [(null? Ns) '()]
      [(eq? n (car Ns)) (car Rs)]
      [else (find-v1 n (cdr Ns) (cdr Rs))])))

(define find-v2
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                [(eq? n (car ns)) (car rs)]
                [else (A (cdr ns) (cdr rs))]))))
        (A Ns Rs))))


(module+ test
  (check-equal? (find-v2 0 '(0 1) '(pepe jaime)) 'pepe)
  (check-equal? (find-v2 2 Ns Rs) '((pizza))))

(define member?
  (lambda (a lat)
    (letrec
        ((?? (lambda (lat)
              (cond
                [(null? lat) #f]
                [(eq? a (car lat)) #t]
                [else (?? (cdr lat))]))))
        (?? lat))))

(define deep-M
  (lambda (n)
    (if (member? n Ns) (find-v2 n Ns Rs) (deep-R2 n))))

(define deep-M2
  (lambda (n)
    (if (member? n Ns) 
        (find-v2 n Ns Rs) 
        (let ((output (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons output Rs))
          output))))

(module+ test
  (check-equal? Ns '(2 0))
  (check-equal? Rs '(((pizza)) pizza))
  (check-equal? (deep-M 1) '(pizza))
  (check-equal? (deep-M 2) '((pizza)))
  (check-equal? (deep-M 3) '(((pizza))))
  (check-equal? (deep-M 4) '((((pizza)))))
  (check-equal? Rs '(((((pizza)))) (((pizza))) (pizza) ((pizza)) pizza)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Ns_3 '(3))
(define Rs_3 '((((pizza)))))

(define find-v3
  (lambda (n Nx Rx)
    (letrec
        ((A (lambda (ns rs)
              (cond
                [(eq? n (car ns)) (car rs)]
                [else (A (cdr ns) (cdr rs))]))))
        (A Nx Rx))))

(define deep-M3
  (lambda (n)
    (if (member? n Ns_3) 
        (find-v3 n Ns_3 Rs_3) 
        (let ((output (deep-v3 n)))
          (set! Ns_3 (cons n Ns_3))
          (set! Rs_3 (cons output Rs_3))
          output))))

(define deep-v3
  (lambda (n)
    (cond 
      [(zero? n) 'pizza]
      [else (cons (deep-M3 (- n 1)) '())])))

(module+ test
  (check-equal? (deep-M3 6) '((((((pizza)))))))
  (check-equal? Ns_3 '(6 5 4 3))
  (check-equal? (deep-M3 9) '(((((((((pizza))))))))))
  (check-equal? Ns_3 '(9 8 7 6 5 4 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define find-v4
  (lambda (n Nx Rx)
    (letrec
        ((A (lambda (ns rs)
              (cond
                [(null? ns) #f]
                [(eq? n (car ns)) (car rs)]
                [else (A (cdr ns) (cdr rs))]))))
        (A Nx Rx))))

(define deep-M4
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (if (member? n Ns) 
          (find-v4 n Ns Rs) 
          (let ((output (deep-v4 n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons output Rs))
            output)))))

(define deep-M5
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find-v4 n Ns Rs)))
            (if (atom? exists)
                (let ((output (deep-v4 n)))
                  (set! Ns (cons n Ns))
                  (set! Rs (cons output Rs))
                  ;(print Ns)
                  output)
                (find-v4 n Ns Rs))))))

(define deep-v4
  (lambda (n)
    (cond 
      [(zero? n) 'pizza]
      [else (cons (deep-M5 (- n 1)) '())])))

(module+ test
  (check-equal? (deep-M5 6) '((((((pizza)))))))
  (check-equal? (deep-M5 9) '(((((((((pizza)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define length-v1
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (+ 1 (length-v1 (cdr l)))])))

(define length-v2
  (lambda (l)
    0))

(set! length-v2
      (lambda (l)
        (cond
          [(null? l) 0]
          [else (+ 1 (length-v2 (cdr l)))])))


