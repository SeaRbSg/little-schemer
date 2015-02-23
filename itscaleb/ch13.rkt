#lang racket

(define member?
  (lambda (a lat)
    (letrec 
        [(M (lambda (lat)
              (cond [(null? lat) #f]
                    (else
                     (or (eq? a (car lat))
                         (M (cdr lat)))))))]
      (M lat))))

(define intersect
  (lambda (set1 set2)
    (letrec
        [(I (lambda (set)
              (cond [(null? set) '()]
                    [(member? (car set) set2)
                     (cons (car set)
                           (I (cdr set)))]
                    [else
                     (I (cdr set))])))]
      (I set1))))

(define intersectall
  (lambda (lset)
    (cond [(null? (cdr lset)) (car lset)]
          [else
           (intersect (car lset)
                      (intersectall (cdr lset)))])))
