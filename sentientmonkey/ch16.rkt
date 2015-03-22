#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch02.rkt")
(require "ch09.rkt")

(provide find)

(define (sweet-tooth food)
  (cons food
    (cons 'cake '())))

(define last 'angelfood)

(test-case "sweet-tooth"
  (check-equal? (sweet-tooth 'chocolate) '(chocolate cake))
  (check-equal? last 'angelfood)
  (check-equal? (sweet-tooth 'fruit) '(fruit cake))
  (check-equal? last 'angelfood))

(define (sweet-toothL food)
  (set! last food)
  (cons food
    (cons 'cake '())))

(test-case "sweet-toothL"
  (check-equal? (sweet-toothL 'chocolate) '(chocolate cake))
  (check-equal? last 'chocolate)
  (check-equal? (sweet-toothL 'fruit) '(fruit cake))
  (check-equal? last 'fruit)
  (check-equal? (sweet-toothL 'cheese) '(cheese cake))
  (check-equal? last 'cheese)
  (check-equal? (sweet-toothL 'carrot) '(carrot cake))
  (check-equal? last 'carrot))

(define ingredients '())

(define (sweet-toothR food)
  (set! ingredients (cons food ingredients))
  (cons food
    (cons 'cake '())))

(test-case "sweet-toothR"
  (check-equal? (sweet-toothR 'chocolate) '(chocolate cake))
  (check-equal? ingredients '(chocolate))
  (check-equal? (sweet-toothR 'fruit) '(fruit cake))
  (check-equal? ingredients '(fruit chocolate))
  (check-equal? (sweet-toothR 'cheese) '(cheese cake))
  (check-equal? ingredients '(cheese fruit chocolate))
  (check-equal? (sweet-toothR 'carrot) '(carrot cake))
  (check-equal? ingredients '(carrot cheese fruit chocolate)))

(define (deep m)
  (if (zero? m)
    'pizza
    (cons (deep (sub1 m)) '())))

(test-case "deep"
  (check-equal? (deep 3) '(((pizza))))
  (check-equal? (deep 7) '(((((((pizza))))))))
  (check-equal? (deep 0) 'pizza))

(define Ns '())

(define (deepR m)
  (set! Ns (cons m Ns))
  (if (zero? m)
    'pizza
    (cons (deep (sub1 m)) '())))

(test-case "deepR"
  (set! Ns '())
  (check-equal? (deepR 3) '(((pizza))))
  (check-equal? Ns '(3))
  (check-equal? (deepR 0) 'pizza))

(define Rs '())

(define (deepR2 m)
  (set! Rs (cons (deep m) Rs))
  (set! Ns (cons m Ns))
  (if (zero? m)
    'pizza
    (cons (deep (sub1 m)) '())))

(test-case "deepR2"
  (set! Ns '())
  (set! Rs '())
  (check-equal? (deepR2 3) '(((pizza))))
  (check-equal? Ns '(3))
  (check-equal? Rs '((((pizza)))))
  (check-equal? (deepR2 0) 'pizza))

(define (deepR3 m)
  (let ([result (deep m)])
    (set! Rs (cons result Rs))
    (set! Ns (cons m Ns))
    result))

(test-case "deepR3"
  (set! Ns '())
  (set! Rs '())
  (check-equal? (deepR3 3) '(((pizza))))
  (check-equal? Ns '(3))
  (check-equal? Rs '((((pizza)))))
  (check-equal? (deepR3 5) '(((((pizza))))))
  (check-equal? Ns '(5 3))
  (check-equal? Rs '((((((pizza))))) (((pizza)))))
  (check-equal? (deepR3 3) '(((pizza))))
  (check-equal? Ns '(3 5 3))
  (check-equal? Rs '((((pizza))) (((((pizza))))) (((pizza))))))

(define (find1 n Ns Rs)
  (letrec
    ([F (lambda (Ns Rs)
          (cond
            [(or (null? Ns) (null? Rs)) '()]
            [(= n (car Ns)) (car Rs)]
            [else (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(test-case "find1"
  (check-equal? (find1 3 Ns Rs) '(((pizza))))
  (check-equal? (find1 5 Ns Rs) '(((((pizza))))))
  (check-equal? (find1 7 Ns Rs) '()))

(define (deepM n)
  (if (member? n Ns)
    (find1 n Ns Rs)
    (deepR n)))

(test-case "deepM"
  (set! Ns '())
  (set! Rs '())
  (deepR3 3)
  (deepR3 5)
  (check-equal? (deepM 3) '(((pizza))))
  (check-equal? (deepM 7) '(((((((pizza)))))))))

(define (deepM2 n)
  (if (member? n Ns)
    (find1 n Ns Rs)
    (let ([result (deep n)])
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(test-case "deepM2"
  (set! Ns '())
  (set! Rs '())
  (check-equal? (deepM2 3) '(((pizza))))
  (check-equal? (deepM2 5) '(((((pizza))))))
  (check-equal? (deepM2 3) '(((pizza))))
  (check-equal? Ns '(5 3))
  (check-equal? Rs '((((((pizza))))) (((pizza)))))
  (check-equal? (deepM2 6) '((((((pizza))))))))

(define (deep2 m)
  (if (zero? m)
    'pizza
    (cons (deepM3 (sub1 m)) '())))

(define (deepM3 n)
  (if (member? n Ns)
    (find1 n Ns Rs)
    (let ([result (deep2 n)])
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(test-case "deepM3"
  (check-equal? (deepM3 9) '(((((((((pizza))))))))))
  (check-equal? Ns '(9 8 7 6 5 3))
  (check-equal? (deepM3 0) 'pizza))

(define deepM4
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (if (member? n Ns)
          (find1 n Ns Rs)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(test-case "deepM4"
  (check-equal? (deepM4 16) '((((((((((((((((pizza)))))))))))))))))
  (check-equal? (deepM4 16) '((((((((((((((((pizza))))))))))))))))))
; Can't test new Rs & Ns because they are inside closure

(define (find n Ns Rs)
  (letrec
    ([F (lambda (Ns Rs)
          (cond
            [(or (null? Ns) (null? Rs)) #f]
            [(= n (car Ns)) (car Rs)]
            [else (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(test-case "find"
  (check-equal? (find 3 '() '()) #f)
  (check-equal? (find 3 '(5 3) '((((((pizza))))) (((pizza))))) '(((pizza)))))

(define deepM5
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

(test-case "deepM5"
  (check-equal? (deepM5 16) '((((((((((((((((pizza)))))))))))))))))
  (check-equal? (deepM4 16) '((((((((((((((((pizza))))))))))))))))))

(define (length l)
  (cond
    [(null? l) 0]
    [else (add1 (length (cdr l)))]))

(define (test-case-length length)
  (test-case "length"
      (check-equal? (length '()) 0)
      (check-equal? (length '(1 2 3 4)) 4)))

(test-case-length length)

(define length-2
  (let ([h 0])
    (set! h
      (lambda (l)
        (cond
          [(null? l) 0]
          [else (add1 (h (cdr l)))])))
    h))

(test-case-length length-2)

(define (L length)
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))

(define length-3
  (let ([h 0])
    (set! h
      (L (lambda (arg) (h arg))))
    h))

(test-case-length length-3)

(define (Y! L)
  (define h
    (L (lambda (arg) (h arg))))
  h)

; Y! is the applicitive order imperative Y-combinator

; Peter J. Landin
; The mechanical evaluation of expressions
; http://www.cs.cmu.edu/afs/cs/user/crary/www/819-f09/Landin64.pdf

(define length-4 (Y! L))

(test-case-length length-4)

(define (Y-bang f)
  (letrec
    ((h (f (lambda (arg) (h arg)))))
    h))

(define length-5 (Y-bang L))

(test-case-length length-5)

(define (D depth*)
  (lambda (s)
    (cond
      [(null? s) 1]
      [(atom? (car s)) (depth* (cdr s))]
      [else (max (add1 (depth* (car s)))
                 (depth* (cdr s)))])))

(define depth* (Y! D))

(test-case "depth*"
  (check-equal? (depth* '()) 1)
  (check-equal? (depth* '(1 1)) 1)
  (check-equal? (depth* '((1 1) (1 1) (1 (2 (3))))) 4))

(define biz
  (let ([x 0])
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
          0
          (f a))))))

(test-case "biz"
  (check-equal? ((Y biz) 5) 0)
; (check-equal? ((Y! biz) 5) 0)
; => infinite
  )
