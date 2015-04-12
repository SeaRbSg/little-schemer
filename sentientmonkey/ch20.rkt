#lang racket/base

(require rackunit)
(require "prelude.rkt")

(define (abort x)
  (error x))


(define (the-empty-table name)
  (abort
    (cons 'no-answer (cons name '()))))

(define (lookup table name)
  (table name))

(define (extend name1 value table)
  (lambda (name2)
    (cond
      [(eq? name2 name1) value]
      [else (table name2)])))

(define (define? e)
  (cond
    [(atom? e) #f]
    [(atom? (car e)) (eq? (car e) 'define)]
    [else #f]))

; box aka bons aka Y!
(define (box it)
  (lambda (sel)
    (sel it (lambda (new)
              (set! it new)))))

(define (setbox box new)
  (box (lambda (it set) (set new))))

(define (unbox box)
  (box (lambda (it set) it)))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (atom-to-action e)
  (cond
    [(number? e)        *const]
    [(eq? e #t)         *const]
    [(eq? e #f)         *const]
    [(eq? e 'cons)      *const]
    [(eq? e 'car)       *const]
    [(eq? e 'cdr)       *const]
    [(eq? e 'null?)     *const]
    [(eq? e 'eq?)       *const]
    [(eq? e 'atom?)     *const]
    [(eq? e 'zero?)     *const]
    [(eq? e 'add1)      *const]
    [(eq? e 'sub1)      *const]
    [(eq? e 'number?)   *const]
    [else *identifier]))

(define (list-to-action e)
  (cond
    [(atom? (car e))
            (cond
              [(eq? (car e) 'quote) *quote]
              [(eq? (car e) 'lambda) *lambda]
              [(eq? (car e) 'letcc) *letcc]
              [(eq? (car e) 'set!) *set]
              [(eq? (car e) 'cond) *cond]
              [else *application])]
     [else *application]))

(define (expression-to-action e)
  (cond
    [(atom? e) (atom-to-action e)]
    [else (list-to-action e)]))

; TODO: lookup John Reynolds
(define (beglis es table)
  (cond
    [(null? (cdr es)) (meaning (car es) table)]
    [else ((lambda (val)
             (beglis (cdr es) table))
           (meaning (car es) table))]))

(define (box-all vals)
  (cond
    [(null? vals) '()]
    [else (cons (box (car vals))
                (box-all (cdr vals)))]))

(define (multi-extend)
  (lambda (names values table)
    (cond
      [(null? names) table]
      [else (extend (car names) (car values)
                    (multi-extend (cdr names) (cdr values) table))])))

(define (evlis args table)
  (cond
    [(null? args) '()]
    [else
      ((lambda (val)
         (cons val
               (evlis (cdr args) table)))
       (meaning (car args) table))]))

(define (:car args-in-a-list)
  (car (car args-in-a-list)))

(define (a-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list)
       (car (cdr args-in-a-list)))))

(define (text-of x)
  (car (cdr x)))

(define (formals-of x)
  (car (cdr x)))

(define (body-of x)
  (cdr (cdr x)))

(define (ccbody-of x)
  (cdr (cdr x)))

(define (name-of x)
  (car (cdr x)))

(define (right-side-of x)
  (cond
    [(null? (cdr (cdr x))) 0]
    [else (car (cdr (cdr x)))]))

(define (cond-lines-of x)
  (cdr x))

(define (else? x)
  (cond
    [(atom? x) (eq? x 'else)]
    [else #f]))

(define (question-of x)
  (car x))

(define (answer-of x)
  (car (cdr x)))

(define (function-of x)
  (car x))

(define (arguments-of x)
  (cdr x))

(define global-table void)

(define (evcon lines table)
  (cond
    [(else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table)]
    [(meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines)) table)]
    [else (evcon (cdr lines) table)]))

(define (*define e)
  (set! global-table
    (extend
      (name-of e)
      (box
        (the-meaning
          (right-side-of e)))
      global-table)))

(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (unbox (lookup table e)))

(define (*set e table)
  (setbox
    (lookup table (name-of e))
    (meaning (right-side-of e) table)))

(define (*lambda e table)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend
              (formals-of e)
              (box-all args)
              table))))

(define (*application e table)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (*const e table)
  (let ([:cons    (b-prim cons)]
        [:car     (a-prim car)]
        [:cdr     (a-prim cdr)]
        [:null?   (b-prim null?)]
        [:eq?     (b-prim eq?)]
        [:atom?   (a-prim atom?)]
        [:number? (a-prim number?)]
        [:zero?   (a-prim zero?)]
        [:add1    (a-prim add1)]
        [:sub1    (a-prim sub1)])
    (cond
    [(number? e)        e]
    [(eq? e #t)         #t]
    [(eq? e #f)         #f]
    [(eq? e '(cons))    :cons]
    [(eq? e '(car))     :car]
    [(eq? e '(cdr))     :cdr]
    [(eq? e '(eq?))     :eq?]
    [(eq? e '(atom?))   :atom?]
    [(eq? e '(null?))   :null?]
    [(eq? e '(zero?))   :zero?]
    [(eq? e '(add1?))   :add1]
    [(eq? e '(sub1?))   :sub1]
    [(eq? e '(number?)) :number?])))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (*letcc e table)
  (let/cc skip
     (beglis (ccbody-of e)
             (extend
               (name-of e)
               (box (a-prim skip))
               table))))

(define (value e)
  (let/cc the-end
      (set! abort the-end)
      (cond
        [(define? e) (*define e)]
        [else (the-meaning e)])))


(test-case "value"
    (check-equal? (value 3) 3)
    (check-equal? (value #t) #t)
    (check-equal? (value #f) #f)
    (value '(define x 3))
    (check-equal? (value 'x) 3)
    (value '(set! x 5))
    (check-equal? (value 'x) 5)
    (check-equal? (value '(add1 x)) 6)
;    (check-equal? (value '((lambda (y)
;                             (set! x 7)
;                             y)
;                           0)) 0)
;    (check-equal? (value 'x) 7)
;     (value '(define ls
;               (cons
;                 (cons
;                   (cons 1 (quote ()))
;                   (quote ()))
;                 (quote ()))))
;     (check-equal? (value '(car (car (car ls)))) 1)
    (check-equal? (value '(cond (else 0))) 0)
;     (check-equal? (value '(cond
;                             ((null? (cons 0 (quote ()))) 0)
;                             (else 1))) 1)
)


