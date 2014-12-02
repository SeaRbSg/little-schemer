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


