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
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l))
                    (skip (car l)))
                   (else
                    (let ()
                      (lm (car l))
                      (lm (cdr l))))))))
        (lm l)))))

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (try oh2
            (cons (rm a (car l) oh2)
                  (cdr l))
            (cons (car l)
                  (rm a (cdr l) oh)))))))

(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (max
        (+ 1 (depth* (car l)))
        (depth* (cdr l)))))))


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