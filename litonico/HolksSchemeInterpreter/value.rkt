#lang r6rs
(import (library (rnrs base (6)))
        (library (rnrs lists (6)))
        (library (rnrs io simple (6))))

;; We're using "association list"s to represent the environment.
;; An association list is a list of cons-pairs, like
;; ((key_a . val_a) (key_b . val_b))
;; and works like an ordered hash.

;; assq (builtin) finds KEY in ALIST using eq?, and returns (KEY . VALUE).

; (define my-assq (key alist)  ; (bite my-shiny-metal-assq)
;   (cond
;     [(null? alist) #f]
;     [(eq? (caar alist) key) (car alist)]
;     [else (my-assq (key (cdr alist)))]))

(define (lookup name env)
  (cdr (assq name env)))

;; An expr is a quoted list, like '(define y 5)
;; `match` matches _exactly_ to expr, so it must be unquoted!
;; If we're passed '(define y 5), we want to manipulate define, not 'define,
;; which is why each line unquotes the expressions.


(define (value-of-1 expr env)
  (match expr
         [,x ; if expr is a plain var, we look up that var in the environment
           (guard (symbol? x)) ; and guard to protect against nonexistance
             (lookup x env)]

         [(lambda (,x) ,expr) ; if env is a lambda expression
             (lambda (v) (value-of-1 expr (cons (cons x v) env)))] ; x to env

         [(,[e1] ,[e2]) ; if x is a two-part expr '(expr1 expr2)
                        ; (the [ ] means to recur on the bracketed
                        ; part of the expr, until...
             (e1 e2)])) ; no more recursion is possible, so apply!
                        ; Note: this seems like magic, what's happening here?!


;; Search through env for x, and then set! x's value to v
(define (update-env! x v env)
  (if [eq? x (caar env)]
    [set-cdr! (car env) v]
    [update-env! x v (cdr env)]))


(define (value-of-2 expr env)
  (match expr
         [,x
           (guard (symbol? x))
             (lookup x env)]

         [,n ; if expr is a number
           (guard (number? n) n)] ; it stays a number!

         ; if expr is a numerical operation (one of + * -),
         ; eval the operation and apply it to the numbers
         [(,num-op ,[e1] ,[e1]) (guard (memq num-op '(+ * -)))
                                      ((eval num-op) e1 e2)]

         [(lambda (,x) ,expr)
             (lambda (v) (value-of-2 expr (cons (cons x v) env)))]

         [(,[e1] ,[e2])
             (e1 e2)]

         [(set! ,x ,[e])
          (update-env! x e env)]

         [(begin ,e* ... ,e) ; match the first n-1 expressions as a list,
          (begin             ; and save the last.
            (let loop ([e* e*])
              (if (pair e*)
                [begin
                  (value-of-2 (car e*) env)
                  (loop (cdr e*))]))
            (value-of-2 e env))]))




(define (debugger env)
  (printf "debug> ")
  (let ([cmd (read)]
        [continue #t])
    (match cmd
           [(continue) (set! continue #f)]
           [,else (printf "unknown command\n")])
    (if continue
        [debugger env]
        [printf "running...\n"])))


(define (show-env env)
  (if (pair? env)
    (let ([depth (show-env (cdr env))])
      (printf "~d. ~a: ~s\n" depth (caar env) (cdar env))
      (+ 1 depth))
    0))


(define (value-of-3 expr env)
  (match expr
         [,x
           (guard (symbol? x))
             (lookup x env)]

         [,n
           (guard (number? n) n)]

         [(,num-op ,[e1] ,[e1]) (guard (memq num-op '(+ * -)))
                                      ((eval num-op) e1 e2)]

         [(lambda (,x) ,expr)
             (lambda (v) (value-of-3 expr (cons (cons x v) env)))]

         [(,[e1] ,[e2])
             (e1 e2)]

         [(set! ,x ,[e])
          (update-env! x e env)]

         [(begin ,e* ... ,e) ; match the first n-1 expressions as a list,
          (begin             ; and save the last.
            (let loop ([e* e*])
              (if (pair e*)
                [begin
                  (value-of-2 (car e*) env)
                  (loop (cdr e*))]))
            (value-of-2 e env))]

         [(debug)
          (debugger env)]

         [(show-env)
          (show_env env)]

         [(eval ,e)
          (printf "~s =? ~s\n" e (value-of-3 e env))]))
