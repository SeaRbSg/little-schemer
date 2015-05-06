;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(require "lib/shared.rkt")

(module+ test
  (require rackunit))

;;; Miscellany

(define debug? #f)

(define (debug name . vals)
  (when debug?
    (printf "~s~n" (cons name vals))))

(define abort #f)                          ; for call/cc

(define global-table #f)

(define (a-prim p)
  (lambda (args-in-a-list) (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list) (p (car args-in-a-list)
                              (car (cdr args-in-a-list)))))

(define (beglis es table)
  (cond [(null? (cdr es)) (meaning (car es) table)]
        [else ((lambda (val) (beglis (cdr es) table))
               (meaning (car es) table))]))

(define (define? e)
  (cond [(atom? e) #f]
        [(atom? (car e)) (eq? (car e) 'define)]
        [else #f]))

(define (evcon lines table)
  (debug 'evcon lines)
  (cond [(else? (question-of (car lines)))
         (meaning (answer-of (car lines))  table)]
        [(meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table)]
        [else (evcon (cdr lines) table)]))

(define (evlis args table)
  (debug 'evlis args)
  (cond [(null? args) '()]
        [else ((lambda (val) (cons val (evlis (cdr args) table)))
               (meaning (car args) table))]))

(define (extend name1 value table)
  (debug 'extend name1)
  (lambda (name2)
    (cond [(eq? name2 name1) value]
          [else (table name2)])))

(define (lookup table name)
  (debug 'lookup name)
  (table name))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  (debug 'meaning e)
  ((expression-to-action e) e table))

(define (multi-extend names values table)
  (cond [(null? names) table]
        [else (extend (car names) (car values)
                      (multi-extend (cdr names) (cdr values) table))]))

;; (define (the-empty-table)
;;   (lambda (name)
;;     (abort (cons 'no-answer (cons name '())))))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (value e)
  (debug 'value e)
  (let/cc the-end
    (set! abort the-end)
    (cond [(define? e) (*define e)]
          [else (the-meaning e)])))

;;; Boxing:

(define (box it)
  (debug 'box it)
  (lambda (sel)
    (sel it (lambda (new) (set! it new)))))

(define (box-all vals)
  (cond [(null? vals) '()]
        [else (cons (box (car vals))
                    (box-all (cdr vals)))]))

(define (setbox box new)
  (box (lambda (it set) (set new))))

(define (unbox box)
  (debug 'unbox box)
  (box (lambda (it set) it)))

;;; Actions:

(define (*application e table)
  (debug 'application e)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (*cond e table)
  (debug 'cond e)
  (evcon (cond-lines-of e) table))

(define (*letcc e table)
  (let/cc skip
    (beglis (ccbody-of e)
            (extend (name-of e)
                    (box (a-prim skip))
                    ))))

(define *const
  (let ([:cons    (b-prim cons)]
        [:car     (a-prim car)]
        [:cdr     (a-prim cdr)]
        [:eq?     (b-prim eq?)]
        [:atom?   (a-prim atom?)]
        [:null?   (a-prim null?)]
        [:zero?   (a-prim zero?)]
        [:add1    (a-prim add1)]
        [:sub1    (a-prim sub1)]
        [:number? (a-prim number?)])
    (lambda (e table)
      (debug 'const e)
      (if (number? e)  e
          (case e
            [(#t)      #t]
            [(#f)      #f]
            [(cons)    :cons]
            [(car)     :car]
            [(cdr)     :cdr]
            [(eq?)     :eq?]
            [(atom?)   :atom?]
            [(null?)   :null?]
            [(zero?)   :zero?]
            [(add1)    :add1]
            [(sub1)    :sub1]
            [(number?) :number?])))))

(define (*define e)
  (debug '*define e)
  (set! global-table (extend (name-of e)
                             (box (the-meaning (right-side-of e)))
                             global-table)))

(define (*identifier e table)
  (debug '*identifier e)
  (unbox (lookup table e)))

(define (*lambda e table)
  (debug '*lambda e)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend (formals-of e)
                          (box-all args)
                          table))))

(define (*quote e table)
  (debug '*quote e)
  (text-of e))

(define (*set e table)
  (debug '*set e)
  (setbox (lookup table (name-of e))
          (meaning (right-side-of e) table)))

;;; Action Dispatch

(define (atom-to-action e)
  (debug 'atom-to-action e)
  (if (number? e)  *const
      (case e
        [(#t)      *const]
        [(#f)      *const]
        [(cons)    *const]
        [(car)     *const]
        [(cdr)     *const]
        [(null?)   *const]
        [(eq?)     *const]
        [(atom?)   *const]
        [(zero?)   *const]
        [(add1)    *const]
        [(sub1)    *const]
        [(number?) *const]
        [else      *identifier])))

(define (expression-to-action e)
  (debug 'expression-to-action e)
  (cond [(atom? e) (atom-to-action e)]
        [else (list-to-action e)]))

(define (list-to-action e)
  (debug 'list-to-action e)
  (cond [(atom? (car e))
         (case (car e)
           [(quote)  *quote]
           [(lambda) *lambda]
           [(letcc)  *letcc]
           [(set!)   *set]
           [(cond)   *cond]
           [else     *application])]
        [else        *application]))

;;; Stupid accessors

(define answer-of     cadr)
(define arguments-of  cdr)
(define body-of       cddr)
(define ccbody-of     cddr)
(define cond-lines-of cdr)
(define formals-of    cadr)
(define function-of   car)
(define name-of       cadr)
(define question-of   car)
(define text-of       cadr)

(define (right-side-of x)
  (cond [(null? (cddr x)) 0]
        [else (caddr x)]))

(define (else? x)
  (cond [(atom? x) (eq? x 'else)]
        [else #f]))

;;; Tests:

(module+ test
  (check-equal? (value 3)
                3)
  (check-equal? (value '(cond [else 0]))
                0)
  (check-equal? (value '(cond [(null? (cons 0 '())) 0] [else 1]))
                1)

  (check-equal? global-table #f)

  (check-equal? (value '(define oddx?
                          (lambda (n)
                            (cond [(zero? n) #f]
                                  [else (evenx? (sub1 n))]))))
                (void))

  (check-equal? (value '(define evenx?
                          (lambda (n)
                            (cond [(zero? n) #t]
                                  [else (oddx? (sub1 n))]))))
                (void))

  (check-equal? (value '(oddx? 2)) #f)
  (check-equal? (value '(oddx? 1)) #t)

  (check-equal? (value '((lambda (n) (oddx? n) (evenx? n)) 3)) #f)

  (check-equal? (value '((lambda (oddx?) (evenx? oddx?)) 3)) #f)

  ;; from ch10:

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
  (check-equal? (value '(cdr (quote (a b c))))
                '(b c))
  (check-equal? (value '(cdr (quote (a b c))))
                '(b c))
  (check-equal? (value '(cdr (quote (a b c))))
                '(b c))
  (check-equal? (value '(eq? 1 2))
                #f)
  (check-equal? (value '(eq? 1 1))
                #t)
  (check-equal? (value '(atom? 1))
                #t)
  (check-equal? (value '(atom? '(1 2 3)))
                #f)
  (check-equal? (value '(number? 1))
                #t)
  (check-equal? (value '(number? '(1 2 3)))
                #f)

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

  (define type expression-to-action)

  ;; (check-equal? (value 'car)
  ;;               (expression-to-action 'car))

  (check-equal? (type 6)                          *const)
  (check-equal? (type #f)                         *const)
  (check-equal? (type 'cons)                      *const)
  ;; (check-equal? (type 'car)                       *const)
  ;; (check-equal? (type 'cdr)                       *const)
  ;; (check-equal? (type 'eq?)                       *const)
  (check-equal? (type 'atom?)                     *const)
  (check-equal? (type '(quote nothing))           *quote)
  (check-equal? (type 'nothing)                   *identifier)
  (check-equal? (type '(lambda (x y (cons x y)))) *lambda)

  (check-equal? (type '((lambda (nothing)
                          (cond [nothing (quote something)]
                                [else (quote nothing)]))
                        #t))
                *application)
  (check-equal? (type '(cond
                         [nothing (quote something)]
                         [else (quote nothing)]))
                *cond)

  ;; (check-equal? (*cond '(cond (coffee klatsch) (else party))
  ;;                      '(((coffee) (#t))
  ;;                        ((klatsch party) (5 (6)))))
  ;;               5)

  (check-equal? (function-of '(cons z x))
                'cons)

  ;; (check-equal? (meaning (function-of '(cons z x)) '())
  ;;               '(primitive cons))

  ;; (check-equal? (apply-closure '((((u v w) (1 2 3))
  ;;                                 ((x y z) (4 5 6)))
  ;;                                (x y)
  ;;                                (cons z x))
  ;;                              '((a b c) (d e f)))
  ;;               '(6 a b c))

  ;; (check-equal? (apply1 '(primitive cons) '(6 (a b c)))
  ;;               '(6 a b c))

  ;; (check-equal? (apply-primitive 'cons '(6 (a b c)))
  ;;               '(6 a b c))

  )
