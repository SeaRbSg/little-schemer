#lang racket
(require rackunit)

;; From previous chapters
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; This chapter

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
     [else *identifier])))

(define list-to-action
  (lambda (l)
    (cond
     [(not (atom? (car l))) *application]
     [(eq? (car l) 'quote) *quote]
     [(eq? (car l) 'lambda) *lambda]
     [(eq? (car l) 'cond) *cond]
     [else *application])))

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

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define initial-table
  (lambda (name)
    (car '())))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond 
     [(else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table)]
     [(meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table)]
     [else (evcon (cdr lines) table)])))

(define else? 
  (lambda (l)
    (and (atom? l) (eq? l 'else))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond [(null? args) '()]
          [else (cons (meaning (car args) table)
                      (evlis (cdr args) table))])))

(define *application
  (lambda (e table)
    (my-apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define my-apply
  (lambda (fun vals)
    (cond
     [(primitive? fun) (apply-primitive (second fun) vals)]
     [(non-primitive? fun) (apply-closure (second fun) vals)])))

(define apply-primitive
  (lambda (name vals)
    (cond
     [(eq? name 'cons) (cons (first vals) (second vals))]
     [(eq? name 'car) (car (first vals))]
     [(eq? name 'cdr) (cdr (first vals))]
     [(eq? name 'eq?) (eq? (first vals) (second vals))]
     [(eq? name 'atom?) (atom? (first vals))]
     [(eq? name 'zero?) (zero? (first vals))]
     [(eq? name 'add1) (add1 (first vals))]
     [(eq? name 'sub1) (sub1 (first vals))]
     [(eq? name 'number? (number? (first vals)))])))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

(define type expression-to-action)
(test-case "type"
           [check-equal? (type 6) 
                         *const]
           [check-equal? (type #f) 
                         *const]
           [check-equal? (type 'cons) 
                         *const]
           [check-equal? (type '(quote nothing)) *quote]
           [check-equal? (type 'nothing) *identifier]
           [check-equal? (type '(lambda (x y) (cons x y))) *lambda]
           [check-equal? (type '((lambda (nothing) (cond (nothing (quote something)) (else (quote nothing)))) #t)) *application]
           [check-equal? (type '(cond (nothing (quote something)) (else (quote onthing)))) *cond]
           [check-equal? (type '(car (quote (a b c)))) *application])

(test-case "value"
           [check-equal? (value 6) 6]
           [check-equal? (value '#f) #f]
           [check-equal? (value 'cons) '(primitive cons)]
           [check-equal? (value '(quote nothing)) 'nothing]
           [check-equal? (value '(quote (car (quote (a b c))))) '(car (quote (a b c)))]
           [check-equal? (value 'car) '(primitive car)]
           [check-equal? (value '(car (quote (a b c)))) 'a]
           [check-equal? (value '(add1 6)) 7]
           [check-equal? (value '((lambda (nothing)
                                    (cond [nothing (quote something)]
                                          [else (quote nothing)])) #t))
                         'something]
           [check-equal? (value '((lambda (nothing) (cons nothing (quote ())))
                         (quote (from nothing comes something))))
                '((from nothing comes something))])

