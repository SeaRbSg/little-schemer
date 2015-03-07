#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
      (yes? lat))))

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) (quote ()))
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop (quote ())))
                  ((null? (cdr lset))
                   (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                          (cond
                            ((null? s1) (quote ()))
                          ((member? (car s1) s2)
                           (cons (car s1) (J (cdr s1))))
                          (else (J (cdr s1)))))))
                (cond
                  ((null? s2) (quote ()))
                  (else (J s1)))))))
        (cond
          ((null? lset) (quote ()))
          (else (A lset)))))))

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a )
                 (quote ()))
                (else (cons (car lat)
                            (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? (car lat) a)
                   (skip (R (cdr lat))))
                  (else
                   (cons (car lat)
                         (R (cdr lat))))))))
        (R lat)))))



(displayln (intersect '(3 mangos and) '(3 hamburgers)))

(displayln (intersectall '((3 mangos and)
                           (3 kiwis and)
                           (3 hamburgers))))

(displayln (intersectall '((3 steaks and)
                           (no food and)
                           (three baked potatoes)
                           (3 diet hamburgers))))

(displayln (intersectall '((3 mangoes and)
                           ()
                           (3 diet hamburgers))))

(displayln (rember-beyond-first '(roots) '(noodles
                                       spaghetti spatzle bean-thread
                                       roots
                                       potatoes yam
                                       others
                                       rice)))

(print (rember-upto-last '(roots) '(noodles
                                    spaghetti spatzle bean-thread
                                    roots
                                    potatoes yam
                                    others
                                    rice)))