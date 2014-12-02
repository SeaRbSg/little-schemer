#lang racket

(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


;; Previous Chapters
(define lat? 
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))


;; Chapter 3
(define rember 
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(test-case "rember"
           (check-equal? (rember 'mint '(lamb chops and mint jelly))
                      '(lamb chops and jelly))
           (check-equal? (rember 'mint '(lamb chops and mint flavored jelly))
                      '(lamb chops and flavored jelly))
           (check-equal? (rember 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato))
           (check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea cup and hick cup))
           (check-equal? (rember 'bacon '(bacon lettuce and tomato))
                      '(lettuce and tomato))
           (check-equal? (rember 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))
           (check-equal? (rember 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce))
           )


(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(test-case "firsts"
           (check-equal? (firsts '((apple peach pumpkin)
                                  (plum pear cherry)
                                  (grape raisin pea)
                                  (bean carrot eggplant)))
                        '(apple plum grape bean))
           (check-equal? (firsts '((a b) (c d) (e f))) '(a c e))
           (check-equal? (firsts '()) '())
           (check-equal? (firsts '((five plums)
                                  (four)
                                  (eleven green oranges)))
                        '(five four eleven))
           (check-equal? (firsts '(((five plums) four)
                                  (eleven green oranges)
                                  ((no) more)))
                        '((five plums) eleven (no)))
           )


(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(test-case "insertR"
           (check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with fudge topping for dessert))
           (check-equal? (insertR 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales and jalapeño salsa))
           (check-equal? (insertR 'e 'd '(a b c d f g d h))
                         '(a b c d e f g d h))
           )


(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))


(test-case "insertL"
           (check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with topping fudge for dessert))
           (check-equal? (insertL 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales jalapeño and salsa))
           (check-equal? (insertL 'e 'd '(a b c d f g d h))
                         '(a b c e d f g d h))
           )


(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(test-case "subst"
           (check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
                         '(ice cream with topping for dessert))
           (check-equal? (subst 'jalapeño 'and '(tacos tamales and salsa))
                         '(tacos tamales jalapeño salsa))
           (check-equal? (subst 'e 'd '(a b c d f g d h))
                         '(a b c e f g d h))
           )


(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(test-case "subst2"
           (check-equal? (subst2 'a 'b 'c '()) '())
           (check-equal? (subst2 'z 'a 'b '(b e a e f)) '(z e a e f))
           (check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
                         '(vanilla ice cream with chocolate topping))
           )


(define multirember 
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(test-case "multirember"
           (check-equal? (multirember 'a '()) '())
           (check-equal? (multirember 'mint '(lamb chops and mint jelly))
                      '(lamb chops and jelly))
           (check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea and hick))
           (check-equal? (multirember 'bacon '(bacon lettuce and tomato))
                      '(lettuce and tomato))
           )


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old 
                                (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(test-case "multiinsertR"
           (check-equal? (multiinsertR 'a 'b '()) '())
           (check-equal? (multiinsertR 'x 'a '(a b a c d)) '(a x b a x c d))
           (check-equal? (multiinsertR 'x 'a '(b a c a d)) '(b a x c a x d))
           )

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new 
                                (cons old (multiinsertL new old (cdr lat)))))
     (else 
      (cons (car lat) (multiinsertL new old (cdr lat)))))))

(test-case "multiinsertL"
           (check-equal? (multiinsertL 'a 'b '()) '())
           (check-equal? (multiinsertL 'x 'a '(a b a c d)) '(x a b x a c d))
           (check-equal? (multiinsertL 'x 'a '(b a c a d)) '(b x a c x a d))
           (check-equal? (multiinsertL 'fried 'fish '(chips and fish or fish and fried)) '(chips and fried fish or fried fish and fried))
           )


(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(test-case "multisubstr"
           (check-equal? (multisubst 'a 'b '()) '())
           (check-equal? (multisubst 'x 'a '(a b a c a)) '(x b x c x))
           (check-equal? (multisubst 'x 'a '(b a c a a)) '(b x c x x))
           )
