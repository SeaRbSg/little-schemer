#lang racket
(require "lib/shared.rkt")
(require rackunit)

;;; page 107

(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)

(test-case "page 107_a" ; Thanks Scott for this great way to test!!
  [check-equal? (sweet-tooth 'chocolate) '(chocolate cake)]
  [check-equal? (sweet-tooth 'fruit) '(fruit cake)]
  [check-equal? last 'angelfood])

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(test-case "page 107_b"
  [check-equal? (sweet-toothL 'chocolate) '(chocolate cake)]
  [check-equal? last 'chocolate])

;;; page 108  

(test-case "page 108"
  [check-equal? (sweet-toothL 'fruit) '(fruit cake)]
  [check-equal? last 'fruit]
  [check-equal? (sweet-toothL 'cheese) '(cheese cake)]
  [check-equal? last 'cheese]
  [check-equal? (sweet-toothL 'carrot) '(carrot cake)]
  [check-equal? last 'carrot])

(define ingredients '())

;;; page 109

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))

(test-case "page 109"
  [check-equal? ingredients '()]
  [check-equal? (sweet-toothR 'chocolate) '(chocolate cake)]
  [check-equal? ingredients '(chocolate)]
  [check-equal? (sweet-toothR 'fruit) '(fruit cake)]
  [check-equal? ingredients '(fruit chocolate)]
  [check-equal? (sweet-toothR 'cheese) '(cheese cake)]
  [check-equal? ingredients '(cheese fruit chocolate)]
  [check-equal? (sweet-toothR 'carrot) '(carrot cake)]
  [check-equal? ingredients '(carrot cheese fruit chocolate)])

;;; page 110 -- and yes, we have broken the outmost holy 16th commandment, Moses would be pissed!

(define deep
  (lambda (m)
    (cond
      [(zero? m) 'pizza]
      [else (cons (deep (sub1 m)) '())])))

(test-case "page 110"
  [check-equal? (deep 3) '(((pizza)))]
  [check-equal? (deep 7) '(((((((pizza)))))))]
  [check-equal? (deep 0) 'pizza])

;;; page 111

(define Ns '())
(define Rs '())

;(define deepR
;  (lambda (n)
;    (set! Ns (cons n Ns))
;    (set! Rs (cons (deep n) Rs))
;    (deep n)))

(define deepR  ; refactored so we compute (deep n) only once
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))

(test-case "page 111"
  [check-equal? Ns '()]
  [check-equal? Rs '()]
  [check-equal? (deepR 3) '(((pizza)))]
  [check-equal? (deepR 7) '(((((((pizza)))))))]
  [check-equal? (deepR 0) 'pizza]
  [check-equal? Ns '(0 7 3)]
  [check-equal? Rs '(pizza (((((((pizza))))))) (((pizza))))])

;;; page 112

(test-case "page 112"
    (set! Ns '())
    (set! Rs '())
    [check-equal? (deepR 3) '(((pizza)))]
    [check-equal? (deepR 5) '(((((pizza)))))]
    [check-equal? (deepR 3) '(((pizza)))]
    [check-equal? Ns '(3 5 3)]
    [check-equal? Rs '((((pizza))) (((((pizza))))) (((pizza))))])

;;; page 113

;(define find
;  (lambda (n Ns Rs)
;    (cond
;      [(null? Ns) #f]
;      [(eq? n (car Ns)) (car Rs)]
;      [else (find n (cdr Ns) (cdr Rs))])))

(define find  ; refactor with letrecc to abide by 12th commandment
  (lambda (n Ns Rs)
    (letrec
     ((A (lambda (enes erres)
         (cond
           [(null? enes) #f]
           [(eq? n (car enes)) (car erres)]
           [else (A (cdr enes) (cdr erres))]))))
     (A Ns Rs))))

(test-case "page 113_a"
  [check-equal? (find 3 '(3 5 3) '((((pizza))) (((((pizza))))) (((pizza))))) '(((pizza)))]
  [check-equal? (find 5 '(3 5 3) '((((pizza))) (((((pizza))))) (((pizza))))) '(((((pizza)))))]
  [check-false  (find 9 '(3 5 3) '((((pizza))) (((((pizza))))) (((pizza)))))]
)

;(define deepM
;  (lambda (n)
;    (let ((gotit (find n Ns Rs)))
;      (cond
;        (gotit gotit)
;        [else 
;         (let ((result (deep n)))
;           (set! Ns (cons n Ns))
;           (set! Rs (cons result Rs))
;           result)]))))

(define deepM  ; simplified although later it goes back to that version
  (lambda (n)
    (let ((gotit (find n Ns Rs)))
      (if gotit gotit (deepR n)))))

(test-case "page 113_b"
    (set! Ns '(3 5 3))
    (set! Rs '((((pizza))) (((((pizza))))) (((pizza)))))
    [check-equal? (deepM 5) '(((((pizza)))))]
    [check-equal? (deepM 3) '(((pizza)))]
    [check-equal? Ns '(3 5 3)]
    [check-equal? Rs '((((pizza))) (((((pizza))))) (((pizza))))])

;;; page 114

(define deepM2 ; without deepR
  (lambda (n)
    (let ((gotit (find n Ns Rs)))
      (if gotit
          gotit
          (let ((result (deep n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons result Rs))
            result)))))

(test-case "page 114"
    (set! Ns '())
    (set! Rs '())
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 5) '(((((pizza)))))]
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 5) '(((((pizza)))))]
    [check-equal? Ns '(5 3)]
    [check-equal? Rs '((((((pizza))))) (((pizza))))])

;;; page 115

(test-case "page 115"
    (set! Ns '())
    (set! Rs '())
    (set! deep (lambda (m)   ; <==== re-defining inside the testcase
            (cond
              [(zero? m) 'pizza]
              [else (cons (deepM2 (sub1 m)) '())])))
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? (deepM2 5) '(((((pizza)))))]
    [check-equal? (deepM2 3) '(((pizza)))]
    [check-equal? Ns '(5 4 3 2 1 0)]
    [check-equal? Rs '((((((pizza))))) ((((pizza)))) (((pizza))) ((pizza)) (pizza) pizza)])

;;; page 116 -- still infringing the 16th!!

(define deepM3
  (let ((Ns '())
        (Rs '())
        (deep (lambda (m)
                (if (zero? m) 
                    'pizza 
                    (cons (deepM3 (sub1 m)) '())))))
    (lambda (n)
      (let ((gotit (find n Ns Rs)))
        (if gotit
            gotit
            (let ((result (deep n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result))))))

(test-case "page 116"
    [check-equal? (deepM3 3) '(((pizza)))]
    [check-equal? (deepM3 5) '(((((pizza)))))]
    [check-equal? (deepM3 3) '(((pizza)))])

;;; page 117 -- already included in my version

;;; page 118 -- don't give a hoot, I like my version more

;;; page 119

;(define length
;  (lambda (l)
;    (if (null? l) 0 (add1 (length (cdr l))))))

(define length
  (lambda (l)
    0))

(set! length
  (lambda (l)
    (if (null? l) 0 (add1 (length (cdr l))))))

(test-case "page 119_a"
  [check-equal? (length '()) 0]
  [check-equal? (length '(1 2)) 2])

; In the name of all that is sacred FIX THE 16th once and for all!!!

(test-case "page 119_b"
  (define length
    (let ((longitud (lambda (l) 0)))
      (set! longitud 
            (lambda (l)
              (if (null? l) 0 (add1 (longitud (cdr l))))))
      longitud))
  [check-equal? (length '()) 0]
  [check-equal? (length '(1 2)) 2])

;;; page 120 & 121 -- shit! now FIX THE 17th!!!

(test-case "page 120 & 121"
  ; given this...
  (define L
    (lambda (fx)
      (lambda (l)
        (if (null? l) 0 (add1 (fx (cdr l)))))))
  
  ; refactor length to use L
  (define length
    (let ((longitud (lambda (l) 0)))
      (set! longitud 
            (L (lambda (x) (longitud x))))
      longitud))
  [check-equal? (length '()) 0]
  [check-equal? (length '(1 2)) 2])

;;; page 122 & 123

(test-case "page 122 & 123"
  (define L
    (lambda (fx)
      (lambda (l)
        (if (null? l) 0 (add1 (fx (cdr l)))))))
  
  ;(define Y
  ;  (lambda (L)
  ;    (let ((h (lambda (l) '())))
  ;    (set! h 
  ;          (L (lambda (x) (h x))))
  ;    h)))
  
  (define Y ; refactor
    (lambda (f)
      (letrec ((h (f (lambda (x) (h x)))))
        h)))
  
  (define length (Y L))
  [check-equal? (length '()) 0]
  [check-equal? (length '(1 2)) 2])
  
; ok, it is time to face the reality of one's limitations and plunge into drunken stupor
