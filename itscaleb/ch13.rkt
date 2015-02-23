#lang racket
(require rackunit)

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
    (letrec [(I (lambda (set)
                  (cond [(null? set) '()]
                        [(member? (car set) set2)
                         (cons (car set)
                               (I (cdr set)))]
                        [else
                         (I (cdr set))])))]
      (I set1))))

(define intersectall
  (lambda (lset)
    (let/cc hop
            (letrec 
                [(I (lambda (lset)
                      (cond [(null? (car lset)) (hop '())]
                            [(null? (cdr lset)) (car lset)]
                            [else
                             (intersect (car lset)
                                        (intersectall (cdr lset)))])))]
              (cond [(null? lset) '()]
                    [else (I lset)])))))

(check-equal? (intersectall '((1 2 3)
                              ()
                              (1 2 3)))
              '())
(check-equal? (intersectall '((1 2 3)
                              (2 4)
                              (1 2 3)))
              '(2))


(define rember-beyond-first
  (lambda (a lat)
    (letrec
        [(R (lambda (lat)
              (cond [(null? lat) '()]
                    [(eq? (car lat) a) '()]
                    [else (cons (car lat)
                                (R (cdr lat)))])))]
      (R lat))))

(check-equal? (rember-beyond-first 'caramel '(cookies chocolate mints caramel delign ginger))
              '(cookies chocolate mints))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
            (letrec [(R (lambda (lat)
                          (cond [(null? lat) '()]
                                [(eq? (car lat) a) (skip (R (cdr lat)))]
                                [else (cons (car lat) (R (cdr lat)))])))]
              (R lat)))))

(check-equal? (rember-upto-last 'roots '(noodles spahgetti spatzle roots potatoes yam others rice))
              '(potatoes yam others rice))
