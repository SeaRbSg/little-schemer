#lang racket

;try + eqlist? from https://github.com/SeaRbSg/little-schemer/blob/master/sotoseattle/ch14.rkt

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (let/cc success 
       (let/cc var (success a)) . b))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)]
      [(null? l2) #f]
      [(eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))]
      [else #f])))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else 
       (let ((a (leftmost (car l))))
       (cond
         ((atom? a) a)
         (else (leftmost (cdr l)))))))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l)
                         (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? (car l) av)
                      (cons (car l) (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (+ 1 (depth* (car l))))
             (d (depth* (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))

(displayln leftmost)
(displayln (leftmost '(((a) b) (cd))))
(displayln (leftmost '(((() a) ()))))
(displayln (leftmost '(((a) ()) () (e))))
(displayln eqlist?)
(displayln (eqlist? '(salad salad salad) '(salad salad salad)))
(displayln (eqlist? '(a b c) '(a b c)))
(displayln (eqlist? '(b c a) '(a b c)))
(displayln rember1*)
(displayln (rember1* 'salad '((Swedish rye)
                                (French (mustard salad turkey))
                                salad)))
(displayln (rember1* 'meat '((pasta meat)
                              pasta
                              (noodles meat sauce)
                              meat tomatoes)))
(displayln depth*)
(displayln (depth* '((pickled) peppers (peppers pickled))))
(displayln (depth* '(margarine
                     ((bitter butter)
                      (makes)
                      (batter (bitter)))
                     butter)))