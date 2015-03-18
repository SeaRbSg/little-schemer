#lang racket/base

(module minimum racket/base
  (provide #%module-begin #%app #%datum #%top require
           prefix-in only-in provide))

(module eval (submod ".." minimum)

  (provide *application
           *cond
           *const
           *identifier
           *lambda
           *quote
           apply-closure
           apply-primitive
           apply1
           expression-to-action
           function-of
           meaning
           value)

  (require (only-in racket/base
                    * + add1 expt number? sub1 zero?
                    and not
                    car cdr cons null? pair?
                    cond else eq?
                    define let let* lambda
                    quote))

  (require (only-in racket/list first second third))

  (define atom?
    (lambda (x)
      (and (not (pair? x))
           (not (null? x)))))

  (define *application
    (lambda (e table)
      (apply1 (meaning (function-of e)  table)
              (evlis   (arguments-of e) table))))

  (define *cond
    (lambda (e table)
      (evcon (cond-lines-of e) table)))

  (define *const
    (lambda (e table)
      (cond [(number? e) e]
            [(eq? e #t) #t]
            [(eq? e #f) #f]
            [else (build 'primitive e)])))

  (define *identifier
    (lambda (e table)
      (lookup-in-table e table initial-table)))

  (define *lambda
    (lambda (e table)
      (build 'non-primitive (cons table (cdr e)))))

  (define *quote
    (lambda (e table)
      (text-of e)))

  (define :atom?
    (lambda (x)
      (cond [(atom? x) #t]
            [(null? x) #f]
            [(eq? (car x) 'primitive) #t]
            [(eq? (car x) 'non-primitive) #t]
            [else #f])))

  (define answer-of   second)

  (define apply-closure
    (lambda (closure vals)
      (meaning (body-of closure)
               (extend-table (new-entry (formals-of closure)
                                        vals)
                             (table-of closure)))))

  (define apply-primitive
    (lambda (name vals)
      (let ((arg1 (first vals)))
        (cond [(eq? name 'cons)    (cons    arg1 (second vals))]
              [(eq? name 'car)     (car     arg1)]
              [(eq? name 'cdr)     (cdr     arg1)]
              [(eq? name 'null?)   (null?   arg1)]
              [(eq? name 'eq?)     (eq?     arg1 (second vals))]
              [(eq? name 'atom?)   (:atom?  arg1)]
              [(eq? name 'zero?)   (zero?   arg1)]
              [(eq? name 'add1)    (add1    arg1)]
              [(eq? name 'sub1)    (sub1    arg1)]
              [(eq? name 'number?) (number? arg1)]))))

  (define apply1
    (lambda (fun vals)
      (cond [(primitive? fun)     (apply-primitive (second fun) vals)]
            [(non-primitive? fun) (apply-closure   (second fun) vals)])))

  (define arguments-of cdr)

  (define atom-to-action
    (lambda (e)
      (cond [(number? e)      *const]
            [(eq? e #t)       *const]
            [(eq? e #f)       *const]
            [(eq? e 'cons)    *const]
            [(eq? e 'car)     *const]
            [(eq? e 'cdr)     *const]
            [(eq? e 'null?)   *const]
            [(eq? e 'eq?)     *const]
            [(eq? e 'atom?)   *const]
            [(eq? e 'zero?)   *const]
            [(eq? e 'add1)    *const]
            [(eq? e 'sub1)    *const]
            [(eq? e 'number?) *const]
            [else             *identifier])))

  (define atom-to-function
    (lambda (x)
      (cond [(eq? x '+) +]
            [(eq? x '*) *]
            [else expt])))

  (define body-of third)

  (define build
    (lambda (s1 s2)
      (cons s1 (cons s2 '()))))

  (define cond-lines-of cdr)

  (define else?
    (lambda (x) (and (atom? x) (eq? x 'else))))

  (define evcon
    (lambda (lines table)
      (let* ((q (question-of (car lines)))
             (a (answer-of   (car lines))))
        (cond [(else? q)         (meaning a table)]
              [(meaning q table) (meaning a table)]
              [else (evcon (cdr lines) table)]))))

  (define evlis
    (lambda (args table)
      (cond [(null? args) '()]
            [else (cons (meaning (car args) table)
                        (evlis (cdr args) table))])))

  (define expression-to-action
    (lambda (e)
      (cond [(atom? e) (atom-to-action e)]
            [else (list-to-action e)])))

  (define extend-table cons)

  (define formals-of second)

  (define function-of car)

  (define initial-table
    (lambda (name)
      (car (quote ()))))

  (define list-to-action
    (lambda (e)
      (cond [(atom? (car e))
             (cond [(eq? (car e) 'quote)  *quote]
                   [(eq? (car e) 'lambda) *lambda]
                   [(eq? (car e) 'cond)   *cond]
                   [else                  *application])]
            [else *application])))

  (define lookup-in-entry
    (lambda (name entry entry-f)
      (lookup-in-entry-help name (first entry) (second entry) entry-f)))

  (define lookup-in-entry-help
    (lambda (name names values entry-f)
      (cond [(null? names)          (entry-f name)]
            [(eq? name (car names)) (car values)]
            [else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)])))

  (define lookup-in-table
    (lambda (name table table-f)
      (cond [(null? table) (table-f name)]
            [else
             (lookup-in-entry name (car table)
                              (lambda (name) (lookup-in-table name
                                                              (cdr table)
                                                              table-f)))])))

  (define meaning
    (lambda (e table)
      ((expression-to-action e) e table)))

  (define new-entry build)

  (define non-primitive?
    (lambda (l)
      (eq? (first l) 'non-primitive)))

  (define primitive?
    (lambda (l)
      (eq? (first l) 'primitive)))

  (define question-of first)

  (define table-of first)

  (define text-of second)

  (define value
    (lambda (e)
      (meaning e '()))))

(module+ test
  (require rackunit)
  (require "lib/shared.rkt")
  (require (submod ".." eval))

  (check-equal? (cons 'a (cons 'b (cons 'c '())))
                '(a b c))

  (define cons-car (cons 'car
                         (cons (cons 'quote
                                     (cons (cons 'a
                                                 (cons 'b
                                                       (cons 'c '())))
                                           '()))
                               '())))
  (check-equal? cons-car '(car (quote (a b c))))
  (check-equal? cons-car '(car '(a b c)))     ; same thing as before

  (check-equal? (car (quote (a b c)))
                'a)
  (check-equal? (value '(car (quote (a b c))))
                'a)
  (check-equal? (value '(quote (car (quote (a b c)))))
                '(car (quote (a b c))))
  (check-equal? (value '(add1 6))
                7)
  (check-equal? (value 7)
                7)
  (check-equal? (value '(quote nothing))
                'nothing)
  (check-equal? (value '((lambda (nothing) (cons nothing (quote ())))
                         (quote (from nothing comes something))))
                '((from nothing comes something)))
  (check-equal? (value '((lambda (nothing)
                           (cond [nothing (quote something)]
                                 [else (quote nothing)]))
                         #t))
                'something)
  (check-false (value #f))
  (check-equal? (value 'car)
                '(primitive car))

  (define type expression-to-action)

  (check-equal? (type 6)
                *const)
  (check-equal? (type #f)
                *const)
  (check-equal? (type 'cons)
                *const)
  (check-equal? (type '(quote nothing))
                *quote)
  (check-equal? (type 'nothing)
                *identifier)
  (check-equal? (type '(lambda (x y (cons x y))))
                *lambda)
  (check-equal? (type '((lambda (nothing)
                          (cond [nothing (quote something)]
                                [else (quote nothing)]))
                        #t))
                *application)
  (check-equal? (type '(cond
                        [nothing (quote something)]
                        [else (quote nothing)]))
                *cond)

  (check-equal? (*cond '(cond (coffee klatsch) (else party))
                       '(((coffee) (#t))
                         ((klatsch party) (5 (6)))))
                5)

  (check-equal? (function-of '(cons z x))
                'cons)
  (check-equal? (meaning (function-of '(cons z x)) '())
                '(primitive cons))

  (check-equal? (apply-closure '((((u v w) (1 2 3))
                                  ((x y z) (4 5 6)))
                                 (x y)
                                 (cons z x))
                               '((a b c) (d e f)))
                '(6 a b c))

  (check-equal? (apply1 '(primitive cons) '(6 (a b c)))
                '(6 a b c))
  (check-equal? (apply-primitive 'cons '(6 (a b c)))
                '(6 a b c)))
