#lang racket
(require rackunit)

;; From previous chapters
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     [(or (null? names) (null? values)) (entry-f name)]
     [(eq? (car names) name) (car values)]
     [else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)])))

(test-case "lookup-in-entry"
           [check-equal? (lookup-in-entry 'entree '((appetizer entree beverage) (food tastes good)) (lambda (name) '())) 'tastes]
           [check-equal? (lookup-in-entry 'dessert '((appetizer entree beverage) (food tastes good)) (lambda (name) 3)) 3])

(define table '(((appetizer entree beverage)
                (pate boeuf vin))
               ((beverage dessert)
                ((food is) (number one with us)))))

(define my-table '((a b c) (1 2 3)))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     [(null? table) (table-f name)]
     [else 
      (lookup-in-entry 
       name 
       (car table) 
       (lambda (name) 
         (lookup-in-table 
          name 
          (cdr table) 
          table-f)))])))

(test-case "lookup-in-table"
           [check-equal? (lookup-in-table 'entree 
                                          '(((entree dessert) 
                                             (spaghetti spumoni)) 
                                            ((appetizer entree beverage) 
                                             (food tastes good))) 
                                          (lambda (name) 5)) 
                         'spaghetti])

(define expression-to-action
  (lambda (e)
    (cond
     [(atom? e) (atom-to-action e)]
     [else (list-to-action e)])))

(define atom-to-action
  (lambda (e)
    (cond
     [(number? e) *const]
     [(eq? e #t) *const]
     [(eq? e #f) *const]
     [(eq? e 'cons) *const]
     [(eq? e 'car) *const]
     [(eq? e 'cdr) *const]
     [(eq? e 'null?) *const]
     [(eq? e 'eq?) *const]
     [(eq? e 'atom?) *const]
     [(eq? e 'zero?) *const]
     [(eq? e 'add1) *const]
     [(eq? e 'sub1) *const]
     [(eq? e 'number?) *const]
     [else '*identifier])))

(define list-to-action
  (lambda (l)
    (cond
     [(not (atom? (car l))) '*application]
     [(eq? (car l) 'quote) '*quote]
     [(eq? (car l) 'lambda) '*lambda]
     [(eq? (car l) 'cond) '*cond]
     [else '*application])))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     [(number? e) e]
     [(eq? e #t) #t]
     [(eq? e #f) #f]
     [else (build 'primitive e)])))

(define type expression-to-action)
(test-case "type"
           [check-eq? (type 6) *const]
           [check-eq? (type #f) *const]
           [check-eq? (type 'cons) *const]
           [check-eq? (type '(quote nothing)) '*quote]
           [check-eq? (type 'nothing) '*identifier]
           [check-eq? (type '(lambda (x y) (cons x y))) '*lambda]
           [check-eq? (type '((lambda (nothing) (cond (nothing (quote something)) (else (quote nothing)))) #t)) '*application]
           [check-eq? (type '(cond (nothing (quote something)) (else (quote onthing)))) '*cond])

(test-case "value"
;;            [check-eq? (value '(car (quote (a b c)))) 'a]
;;            [check-eq? (value '(quote (car (quote (a b c))))) '(car (quote (a b c)))]
;;            [check-eq? (value '(add1 6)) 7]
           [check-eq? (value 6) 6]
;;            [check-eq? (value '(quote nothing)) 'nothing]
;;            [check-eq? (value 'nothing) '()]
;;            [check-qual? (value '((lambda (nothig) (cons nothing (quote ())))) (quote (from nothing comes something))) '((from nothing comes something))]
           [check-eq? (value '#f) #f]
           [check-equal? (value 'car) '(primitive car)]
           )

