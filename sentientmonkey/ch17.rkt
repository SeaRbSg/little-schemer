#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch16.rkt")


(define counter '())
; had to initialize counter to something first
(define set-counter '())

(define consC
  (let ([N 0])
    (set! counter (lambda () N))
    (set! set-counter (lambda (n) (set! N n)))
    (lambda (a b)
      (set! N (add1 N))
      (cons a b))))

(define (deep m)
  (if (zero? m)
    'pizza
    (consC (deep (sub1 m)) '())))

(test-case "deep"
  (check-equal? (deep 5) '(((((pizza))))))
  (check-equal? (counter) 5)
  (check-equal? (deep 7) '(((((((pizza))))))))
  (check-equal? (counter) 12))

(define (supercounter f)
  (letrec ([S (lambda (n)
                (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))])
    (S 1000)
    (counter)))

(test-case "supercounter"
  (check-equal? (supercounter deep) 500512)
  (set-counter 0)
  (check-equal? (supercounter deep) 500500))

; Carl F. Guass
; http://www.math.wichita.edu/history/Men/gauss.html

(define (teacher)
  (letrec
    ([T (lambda (n)
          (if (zero? n)
            0
            (+ n (T (sub1 n)))))])
    (T 100)))

(define (guass)
  (* 50 101))

(test-case "teacher vs guass"
  (check-equal? (teacher) 5050)
  (check-equal? (guass) 5050))

; "A LISP programmer knows the value of everything but the cost of nothing"
; - Alan J. Perlis

; More Epigrams: http://pu.inf.uni-tuebingen.de/users/klaeren/epigrams.html
; My favorite is "Programming is an unnatural act."

(define deepM
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
          (let ([result
                  (if (zero? n)
                    'pizza
                    (consC (deepM (sub1 n)) '()))])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

(test-case "deepM"
  (set-counter 0)
  (check-equal? (deepM 5) '(((((pizza))))))
  (check-equal? (counter) 5)
  (check-equal? (deepM 7) '(((((((pizza))))))))
  (check-equal? (counter) 7))

(define (rember1*C a l)
  (letrec ([R (lambda (l oh)
                (cond [(null? l) (oh 'no)]
                      [(atom? (car l))
                         (if (eq? (car l) a)
                           (cdr l)
                           (consC (car l) (R (cdr l) oh)))]
                      [else (let ([new-car
                                    (let/cc oh
                                            (R (car l) oh))])
                              (if (atom? new-car)
                                (consC (car l) (R (cdr l) oh))
                                (consC new-car (cdr l))))]))])
    (let ([new-l (let/cc oh (R l oh))])
      (if (atom? new-l)
        l
        new-l))))

(test-case "rember1*C a l"
  (set-counter 0)
  (check-equal? (rember1*C 'noodles '((food) more (food))) '((food) more (food)))
  (check-equal? (counter) 0))

(test-case "consC consC consC"
    (set-counter 0)
    (check-equal? (let ([f 'food]
                        [m 'more])
                    (consC (consC f '())
                           (consC m
                                  (consC (consC f '()) '())))) '((food) more (food)))
    (check-equal? (counter) 5))

(define (rember1*C2 a l)
  (letrec
    ([R (lambda (l)
          (cond
            [(null? l) '()]
            [(atom? (car l))
             (if (eq? (car l) a)
               (cdr l)
               (consC (car l) (R (cdr l))))]
            [else
              (let ([av (R (car l))])
                (if (eq? av (car l))
                  (consC (car l) (R (cdr l)))
                  (consC av (cdr l))))]))])
    (R l)))

(test-case "rember1*C2"
  (set-counter 0)
  (check-equal? (rember1*C2 'noodles '((food) more (food))) '((food) more (food)))
  (check-equal? (counter) 2)) ; <- why is this 2 and not 5??

; I'm in the mood for lamb. Good thing I'm going to Lola tonight! :)
