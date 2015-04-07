#lang racket
(require rackunit)

;; Previous chapters
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define deep
  (lambda (m)
    (cond 
     [(zero? m) 'pizza]
     [else (cons (deep (sub1 m)) '())])))

(test-case "deep"
           [check-equal? (deep 6) '((((((pizza))))))])



;; This chapter

(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p '())
         '())
        '())
       '())
      '())
     '())))

(test-case "six-layers"
           [check-equal? (six-layers 'mozzarella) '((((((mozzarella))))))])

(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())))

(test-case "four-layers"
           [check-equal? (four-layers 'chicken) '((((chicken))))])


(define toppings void)

(define deepB
  (lambda (m)
    (cond
     [(zero? m)
      (let/cc jump
              (set! toppings jump)
              'pizza)]
     [else (cons (deepB (sub1 m)) '())])))

;; DISCUSSION PLEASE I'M SO CONFUSED
(deepB 6) ;; => '((((((pizza))))))
(toppings 'mozzarella) ;; => '((((((mozzarella)
(toppings 'cake) ;; => '((((((cake))))))

;; Random crazy oddness. By itself the first line passes. If I add the second line the first line fails. 

;; [check-equal? (deepB 6) '(((((((pizza)))))))]
;; [check-equal? (toppings 'mozzarella) '((((((mozzarella))))))]

;; Ryan explained this to me. let/cc saves a continuation, an execution frame.
;; The first line passes on its own. Calling toppings on the second line rexecutes the continuation from the original call to deepB and therefore changes its result. So it passes once and runs again and then it fails. 
;; CONTINUATIONS HOW DO THEY WORK!?!?

(cons (toppings 'cake) '()) ;; => '((((((cake))))))
(cons (cons (cons (toppings 'mozzarella) '()) '()) '()) ;; => '((((((mozzarella))))))

(deepB 4) ;; =>'((((pizza))))
(cons (cons (cons (toppings 'mozzarella) '()) '()) '()) ;; => '((((mozzarella))))
(cons (toppings 'cake) (toppings 'cake)) ;; => '((((cake))))

(cons (toppings 'cake)
      (cons (toppings 'mozzarella)
            (cons (toppings 'pizza) '()))) ;; => '((((cake))))

(define I 
  (lambda (x) x))

(test-case "I"
           [check-equal? (I 'pizza) 'pizza])

(define deep&co
  (lambda (m k)
    (cond
     [(zero? m) (k 'pizza)]
     [else 
      (deep&co (sub1 m)
               (lambda (x) (k (cons x '()))))])))

(test-case "deep&co"
           [check-equal? (deep&co 0 I) 'pizza]
           [check-equal? (deep&co 6 I) '((((((pizza))))))]
           [check-equal? (deep&co 2 I) '((pizza))])

(define deep&coB
  (lambda (m k)
    (cond
     [(zero? m)
      (let ()
        (set! toppings k)
        (k 'pizza))]
     [else
      (deep&coB (sub1 m)
                (lambda (x)
                  (k (cons x '()))))])))

(deep&coB 4 I) ;; => '((((pizza))))
(cons (toppings 'cake) (toppings 'cake)) ;; => '(((((cake)))) (((cake))))
;; Question: Why is the second one only surrounded by 3 parens?

(cons (toppings 'cake)
      (cons (toppings 'mozzarella)
            (cons (toppings 'pizza) '())))
;; => '(((((cake)))) ((((mozzarella)))) ((((pizza)))))

;; From chapter 11
(define two-in-a-row?
  (lambda (lat)
    (cond
     [(null? lat) #f]
     [else (two-in-a-row-b? (car lat) (cdr lat))])))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (or (eq? (car lat) a)
               (two-in-a-row-b? (car lat) (cdr lat)))])))

(test-case "two-in-a-row?"
           [check-false (two-in-a-row? '(mozzarella cake mozzarella))]
           [check-true  (two-in-a-row? '(mozzarella mozzarella cake))])

(define leave void)

(define walk
  (lambda (l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (leave (car l))]
     [else
      (let ()
        (walk (car l))
        (walk (cdr l)))])))

(define fill void)

(define waddle
  (lambda (l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) 
      (let ()
        (let/cc rest
                (set! fill rest)
                (leave (car l)))
        (waddle (cdr l)))]
     [else
      (let ()
        (walk (car l))
        (walk (cdr l)))])))


(test-case "two-in-a-row*?"
           [check-false (two-in-a-row*? '((mozz) (cake) mozza))]
           [check-true  (two-in-a-row*? '((potato) (chips ((with) fish) (fish))))]
           [check-false (two-in-a-row*? '((potato) (chips ((with) fish) (chips))))]
           [check-true  (two-in-a-row*? '((potato) (chips (chips (with) fish))))])


