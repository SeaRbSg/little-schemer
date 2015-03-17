#lang racket
(require rackunit)

;; From other chapters
(define member?
  (lambda (a lat)
    (cond 
     [(null? lat) #f]
     [else (or (eq? (car lat) a)
               (member? a (cdr lat)))])))


;; This chapter
(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake '()))))

(define last 'angelfood)

(test-case "sweet-tooth"
           [check-equal? (sweet-tooth 'chocolate) '(chocolate cake)]
           [check-equal? (sweet-tooth 'fruit) '(fruit cake)])

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(test-case "sweet-toothL"
           [check-equal? (sweet-toothL 'chocolate) '(chocolate cake)]
           [check-equal? last 'chocolate]
           [check-equal? (sweet-toothL 'fruit) '(fruit cake)]
           [check-equal? last 'fruit]
           [check-equal? (sweet-toothL 'cheese) '(cheese cake)]
           [check-equal? last 'cheese]
           [check-equal? (sweet-toothL 'carrot) '(carrot cake)]
           [check-equal? last 'carrot])

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))

(test-case "sweet-toothR"
           [check-equal? (sweet-toothR 'chocolate) '(chocolate cake)]
           [check-equal? ingredients '(chocolate)]
           [check-equal? (sweet-toothR 'fruit) '(fruit cake)]
           [check-equal? ingredients '(fruit chocolate)]
           [check-equal? (sweet-toothR 'cheese) '(cheese cake)]
           [check-equal? ingredients '(cheese fruit chocolate)]
           [check-equal? (sweet-toothR 'carrot) '(carrot cake)]
           [check-equal? ingredients '(carrot cheese fruit chocolate)])

(define deep
  (lambda (m)
    (cond
     [(zero? m) 'pizza]
     [else (cons (deep (sub1 m)) '())])))

(test-case "deep"
           [check-equal? (deep 3) '(((pizza)))]
           [check-equal? (deep 7) '(((((((pizza)))))))]
           [check-equal? (deep 0) 'pizza])


(define Ns '())
(define Rs '())

(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))

(test-case "deepR"
           [check-equal? (deepR 3) '(((pizza)))]
           [check-equal? (deepR 5) '(((((pizza)))))]
           [check-equal? (deepR 3) '(((pizza)))]
           [check-equal? (deepR 0) 'pizza])

(define find-helper-index '(4 5 6))
(define find-helper-data  '(a b c))

(define find
  (lambda (n Ns Rs)
    (letrec 
        ((F (lambda (ns rs)
            (cond
             [(null? ns) #f]
             [(eq? n (car ns)) (car rs)]
             [else (F (cdr ns) (cdr rs))]))))
       (F Ns Rs))))

(test-case "find"
           [check-equal? (find 4 find-helper-index find-helper-data) 'a]
           [check-equal? (find 6 find-helper-index find-helper-data) 'c])


(set! Ns '())
(set! Rs '())

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))

(test-case "deepM"
           [check-equal? (deepM 3) '(((pizza)))]
           [check-equal? (deepM 5) '(((((pizza)))))]
           [check-equal? (deepM 3) '(((pizza)))]
           [check-equal? (deepM 0) 'pizza]
           [check-equal? (deepM 6) '((((((pizza))))))])

(define deep2
  (lambda (m)
    (cond
     [(zero? m) 'pizza]
     [else (cons (deepM (sub1 m)) '())])))

(define deepM2
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep2 n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))

(test-case "deepM2"
            [check-equal? (deepM2 9) '(((((((((pizza)))))))))]
            [check-equal? Ns '(9 8 6 0 5 3)])

(define deep3
  (lambda (m)
    (cond
     [(zero? m) 'pizza]
     [else (cons (deepM3 (sub1 m)) '())])))

(define deepM3
  (let ((Rs '()) 
        (Ns '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep3 n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons result Rs))
            result)))))

(test-case "deepM3"
            [check-equal? (deepM3 2) '((pizza))]
            [check-equal? Ns '(9 8 6 0 5 3)])

(set! Ns '())
(set! Rs '())

(test-case "make-pizza"
           [check-equal? (deepM2 16) '((((((((((((((((pizza))))))))))))))))])


;; PIZZA BREAK

;; (define length
;;   (lambda (l) 0))

;; (set! length
;;   (lambda (l)
;;     (cond
;;      [(null? l) 0]
;;      [else (add1 (length (cdr l)))])))

;; (define length
;;   (let ((h (lambda (l) 0)))
;;     (set! h
;;           (lambda (l)
;;             (cond
;;              [(null? l) 0]
;;              [else (add1 (h (cdr l)))])))))

(define L
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg)))) h))

(define Y_!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg))))) h)))

(define absurd_length (Y_! L))
