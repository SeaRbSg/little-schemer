#lang racket
(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rm
  (lambda (a l oh)
    (cond
     [(null? l) (oh 'no)]
     [(atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l) (rm a (cdr l) oh)))]
     [else (if (atom?
                (let/cc oh
                        (rm a (car l) oh)))
               (cons (car l) (rm a (cdr l) oh))
               (cons (rm a (car l) 0) (cdr l)))])))

(define rember1*
  (lambda (a l)
    (if (atom? (let/cc oh (rm a l oh)))
        l
        (rm a l '()))))

(check-equal? (rember1* 'noodles '((food) more (food)))
              '((food) more (food)))
