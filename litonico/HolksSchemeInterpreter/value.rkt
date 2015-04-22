#lang r6rs

;; We're using "association list"s to represent the environment.
;; An association list is a list of cons-pairs, like
;; '((key_a . val_a) (key_b . val_b))
;; and works just like an ordered hash.

;; assq (builtin) finds KEY in ALIST using eq?, and returns (KEY . VALUE).

; (define my-assq (key alist)  ; (bite my-shiny-metal-assq)
;   (cond
;     [(null? alist) #f]
;     [(eq? (cddr alist) key) (cdr alist)]
;     [else (my-assq (key (cdr alist)))]))

(define (lookup name env)
  (cdr (assq name env)))

;; An expr is a quoted list, like '(define y 5)
;; `match` matches _exactly_ to expr, so it must be unquoted!
;; If we're passed '(define y 5), we want to match define, not 'define

(define (value-of expr env)
  (match expr
         [,x ; if expr is a plain var, we look up that var in the environment
           (guard (symbol? x)) ; and guard to protect against nonexistance
             (lookup x env)]
         [(lambda (,x) ,expr) ; if env is a lambda expression
             (lambda (v) (value-of expr (cons (cons x v) env)))] ; add x to env
         [(,[e1] ,[e2]) ; if x is a two-part expr '(expr1 expr2)
             (e1 e2)]))
