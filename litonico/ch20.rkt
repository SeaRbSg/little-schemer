#lang racket/base

;; table: a hash
;; name: a key in a hash
;; value: the value in a hash
;; meaning: ???

; (define the-empty-table
;  (lambda name))

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        [(eq? name2 name1) value]
        [else (table name2)]))))

(define value
  (lambda (e)
    ;...
    (cond
      [(define? e) (*define e)]
      [else (the-meaning e)])))

(define global-table the-empty-table)

(define *define
  (lambda (e)
    (set! global-table
      (extend
        (name-of e)
        (box
          (the meaning
               (right-side-of e)))
        global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

(define meaning
  (lambda (e table)
    ((expression-to-action e)
     e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define beglis
  (lambda (es table)
    (cond
      [(null? (cdr es))
       (meaning (car es) table)]
      [else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))])))

(define box-all
  (lambda (vals)
    (cond
      [(null? vals) '()]
      [else (cons (box (car vals))
                  (box-all (cdr vals)))])))

 ;; Yay, the let vs. lambda thing!

(define multi-extend
  (lambda (names values table)
    (cond
      [(null? names) table]
      [else
        (extend (car names) (car values)
                (multi-extend
                  (cdr names)
                  (cdr values)
                  table))])))

(define odd?
  (lambda (n)
    (cond
      [(zero? n) #f]
      [else (even? (sub1 n))])))

(define even?
  (lambda (n)
    (cond
      [(zero? n) #t]
      [else (odd? (sub1 n))])))

(define :car
  (lambda (argslist)
    (car (car argslist)))) ; what?


(define a-prim
  (lambda (p)
    (lambda (argslist)
      (p (car argslist)))))

(define b-prim
  (lambda (p)
    (lambda (argslist)
      (p (car argslist)
         (car (cdr argslist))))))
