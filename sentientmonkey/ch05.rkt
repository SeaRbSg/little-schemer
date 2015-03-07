#lang racket
(require rackunit)
(require "prelude.rkt")
(require "ch02.rkt")
(require "ch04.rkt")

(provide eqlist?)

; Chapter 4
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))

(check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
              '(((tomato)) ((bean)) (and ((flying)))))

(check-equal? (lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) #f)

(check-equal? (atom? (car '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))) #f)

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(check-equal? (insertR* 'roast 'chuck
                       '(((how much (wood))
                              could
                              ((a (wood) chuck))
                              (((chuck)))
                               (if (a) ((wood chuck)))
                               could chuck wood)))
                       '(((how much (wood))
                         could
                         ((a (wood) chuck roast))
                         (((chuck roast)))
                          (if (a) ((wood chuck roast)))
                          could chuck roast wood)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(check-equal? (occur* 'banana '(((banana)
                                 (split ((((banana ice))
                                          (cream (banana))
                                          sherbert))
                                        (banana)
                                        (bread)
                                        (banana brandy))))) 5)

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(check-equal? (subst* 'orange 'banana '(((banana)
                                        (split ((((banana ice)))
                                                (cream (banana))
                                                sherbert))
                                        (banana)
                                        (bread)
                                        (banana brandy))))
              '(((orange)
                 (split ((((orange ice)))
                         (cream (orange))
                         sherbert))
                 (orange)
                 (bread)
                 (orange brandy))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(check-equal? (insertL* 'pecker 'chuck '(((how much (wood))
                                         could
                                         ((a (wood) chuck))
                                         (((chuck)))
                                         (if (a) ((wood chuck)))
                                         could chuck wood)))
              '(((how much (wood))
                 could
                 ((a (wood) pecker chuck))
                 (((pecker chuck)))
                 (if (a) ((wood pecker chuck)))
                 could pecker chuck wood)))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(check-equal? (member* 'chips '((potato) (chips ((with) fish) (chips)))) #t)

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(check-equal? (leftmost '((potato) (chops ((with fish) (chips))))) 'potato)

(check-equal? (and (atom? (car '(mozzarella pizza))) (eq? (car '(mozzarella pizza)) 'pizza)) #f)
(check-equal? (and (atom? (car '((mozzarella mushroom) pizza))) (eq? (car '((mozzarella mushroom) pizza)) 'pizza)) #f)
(check-equal? (and (atom? (car '(mozzarella pizza))) (eq? (car '(mozzarella pizza)) 'mozzarella)) #t)

; first attempt
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2))
                          (eqlist? (cdr l1) (cdr l2)))))))

(check-equal? (eqlist? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-equal? (eqlist? '(bananna ((split))) '((banana) (split))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #t)

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2) #f))
      (else (eqlist? s1 s2)))))

(check-equal? (equal? 'atom 'atom) #t)
(check-equal? (equal? 'atom 'anotheratom) #f)
(check-equal? (equal? '(a list of atoms) '(a list of atoms)) #t)
(check-equal? (equal? '(a list of atoms) '(a different  list of atoms)) #f)
(check-equal? (equal? '(a (nested) list of atoms) '(a (nested) list of atoms)) #t)
(check-equal? (equal? '(a (nested) list of atoms) '(a different (nested) list of atoms)) #f)

; rewrite of eqlist?
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l2) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist2? (cdr l1) (cdr l2)))))))

(check-equal? (eqlist2? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-equal? (eqlist2? '(bananna ((split))) '((banana) (split))) #f)
(check-equal? (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #f)
(check-equal? (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #t)

; rember simplified
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

(check-equal? (rember 'turkey '(ham turkey and cheese sandwhich)) '(ham and cheese sandwhich))
