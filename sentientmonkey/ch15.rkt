#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch02.rkt")

(define x
  (cons 'chicago
    (cons 'pizza
      '())))

(check-equal? x '(chicago pizza))

(set! x 'gone)

(check-equal? x 'gone)

(set! x 'skins)

(check-equal? x 'skins)

(define (gourmet food)
  (cons food
    (cons x '())))

(check-equal? (gourmet 'onion) '(onion skins))

(set! x 'rings)

(check-equal? (gourmet 'onion) '(onion rings))

(define (gourmand food)
  (set! x food)
  (cons food
    (cons x '())))

(test-case "gourmand"
  (check-equal? (gourmand 'potato) '(potato potato))
  (check-equal? x 'potato)
  (check-equal? (gourmand 'rice) '(rice rice))
  (check-equal? x 'rice))

(define (diner food)
  (cons 'milkshake
    (cons food '())))

(check-equal? (diner 'onion) '(milkshake onion))

(define (dinerR food)
  (set! x food)
  (cons 'milkshake
    (cons food '())))

(test-case "dinerR"
  (check-equal? (dinerR 'onion) '(milkshake onion))
  (check-equal? x 'onion)
  (check-equal? (dinerR 'pecanpie) '(milkshake pecanpie))
  (check-equal? x 'pecanpie)
  (check-equal? (gourmand 'onion) '(onion onion)))

(define omnivore
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

(test-case "omnivore"
  (check-equal? (omnivore 'bouillabaisse) '(bouillabaisse bouillabaisse))
  (check-equal? x 'onion))

(define gobbler
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

(test-case "gobbler"
  (check-equal? (gobbler 'gumbo) '(gumbo gumbo))
  (check-equal? x 'onion))

(define food '()) ; had to define food before set!ing the value

(define (glutton x)
  (set! food x)
  (cons 'more
    (cons x
      (cons 'more
        (cons x '())))))

(test-case "glutton"
  (check-equal? (glutton 'onion) '(more onion more onion))
  (check-equal? food 'onion)
  (check-equal? (glutton 'garlic) '(more garlic more garlic))
  (check-equal? food 'garlic)
  (check-equal? x 'onion))

(define (chez-nous)
  (let ([old-food food])
    (set! food x)
    (set! x old-food)))

(test-case "chez-nous"
  (chez-nous)
  (check-equal? food 'onion)
  (check-equal? x 'garlic))

(test-case "more chez-nous"
  (check-equal? (glutton 'garlic) '(more garlic more garlic))
  (check-equal? (gourmand 'potato) '(potato potato))
  (check-equal? x 'potato)
  (chez-nous)
  (check-equal? food 'potato)
  (check-equal? x 'garlic))

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

(define (find n Ns Rs)
  (letrec
    ([F (lambda (Ns Rs)
          (cond
            [(or (null? Ns) (null? Rs)) '()]
            [(= n (car Ns)) (car Rs)]
            [else (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(test-case "find"
  (check-equal? (find 3 Ns Rs) '(((pizza))))
  (check-equal? (find 5 Ns Rs) '(((((pizza))))))
  (check-equal? (find 7 Ns Rs) '()))

(define (deepM n)
  (if (member? n Ns)
    (find n Ns Rs)
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
    (find n Ns Rs)
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
    (find n Ns Rs)
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
          (find n Ns Rs)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(test-case "deepM4"
  (check-equal? (deepM4 16) '((((((((((((((((pizza)))))))))))))))))
  (check-equal? (deepM4 16) '((((((((((((((((pizza))))))))))))))))))
; Can't test new Rs & Ns because they are inside closure

(define (find2 n Ns Rs)
  (letrec
    ([F (lambda (Ns Rs)
          (cond
            [(or (null? Ns) (null? Rs)) #f]
            [(= n (car Ns)) (car Rs)]
            [else (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(test-case "find2"
  (check-equal? (find2 3 '() '()) #f)
  (check-equal? (find2 3 '(5 3) '((((((pizza))))) (((pizza))))) '(((pizza)))))

(define deepM5
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find2 n Ns Rs)])
        (if (atom? exists)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

(test-case "deepM5"
  (check-equal? (deepM5 16) '((((((((((((((((pizza)))))))))))))))))
  (check-equal? (deepM4 16) '((((((((((((((((pizza))))))))))))))))))

(define length
  (let ([h (lambda (l) 0)])
    (set! h
      (lambda (l)
        (cond [(null? l) 0]
              [else (add1 ((lambda (arg) (h arg)) (cdr l)))])))
    h))

(test-case "length"
  (check-equal? (length '()) 0)
  (check-equal? (length '(1 2 3 4)) 4))
