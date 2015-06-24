;; -*- mode: racket; comment-column: 70 -*-

#lang racket/base

(require racket/dict)
(require (prefix-in real/ "lib/unreasonable.rkt"))

;; (struct var (name) #:transparent)

(define var vector)
(define var? vector?)
(define rhs cdr)
(define lhs car)

(define empty-s '())

;; TODO: see identifier macros in Scheme programming lang, dybvig, pg 193, 204

;; original version from scratch, ugly but direct.
(define (walk/1 v l)
  (let ([match (assoc v l)])
    (if match
        (let ([match (rhs match)])
          (if (var? match)
              (walk/1 match l)
              match))
        v)))

(define (var-get v l)
  (dict-ref l v #f))

(define (walk/2 v l)
  (let ([match (var-get v l)])
    (cond [(var? match) (walk/2 match l)]
          [match]
          [else v])))

;; this version requires an extra recursion to finally resolve
;; but that cond => form is elegant
(define (walk/3 v l)
  (cond [(and (var? v) (assq v l)) => (lambda (a) (walk/3 (rhs a) l))]
        [else v]))

(define (walk/4 v l)
  (cond [(and (var? v) (var-get v l)) => (lambda (a) (walk/4 a l))]
        [else v]))

(define walk walk/4)

(define (ext-s x v s)                                             ; 29
  (cons `(,x . ,v) s))

(define (unify v w s)                                             ; 36
  (let ([v (walk v s)]
        [w (walk w s)])
    (cond [(eq? v w) s]
          [(var? v) (ext-s v w s)]
          [(var? w) (ext-s w v s)]
          [(and (pair? v) (pair? w))
           (cond [(unify (car v) (car w) s) => (lambda (s) (unify (cdr v) (cdr w) s))]
                 [else #f])]
          [(equal? v w) s]
          [else #f])))

(define (walk*/1 v s)                                             ; 47
  (let ([v (walk v s)])
    (cond [(var? v) v]
          [(pair? v) (cons (walk*/1 (car v) s)
                           (walk*/1 (cdr v) s))]
          [else v])))

(define walk* walk*/1)

(define-syntax project                                            ; 47
  (syntax-rules ()
    [(_ (x ...) g ...)
     (real/λg (s) (let ([x (walk* x s)] ...)
                    ((real/all g ...) s)))]))

(define (reify-name n)                                            ; 52
  (string->symbol (format "_.~s" n)))

(define (reify-s v s)                                             ; 52
  (define size-s length)
  (let ([v (walk v s)])
    (cond [(var? v) (ext-s v (reify-name (size-s s)) s)]
          [(pair? v) (reify-s (cdr v)
                              (reify-s (car v) s))]
          [else s])))

(define (reify v)                                                 ; 58
  (walk* v (reify-s v empty-s)))

(define (occurs√ x v s)                                           ; 59
  (let ([v (walk v s)])
    (cond [(var? v) (eq? v x)]
          [(pair? v) (or (occurs√ x (car v) s)
                         (occurs√ x (cdr v) s))]
          [else #f])))

(define (ext-s√ x v s)                                            ; 59
  (cond [(occurs√ x v s) #f]
        [else (ext-s x v s)]))

(define (unify√ v w s)                                            ; 59
  (let ([v (walk v s)]
        [w (walk w s)])
    (cond [(eq? v w) s]
          [(var? v) (ext-s√ v w s)]
          [(var? w) (ext-s√ w v s)]
          [(and (pair? v) (pair? w))
           (cond [(unify√ (car v) (car w) s) =>
                  (lambda (s) (unify√ (cdr v) (cdr w) s))]
                 [else #f])]
          [(equal? v w) s]
          [else #f])))

(module+ test
  (require rackunit)
  (require (submod "lib/unreasonable.rkt" test))

  (define u (var 'u))
  (define v (var 'v))
  (define w (var 'w))
  (define x (var 'x))
  (define y (var 'y))
  (define z (var 'z))

  (check-equal? (rhs `(,z . b)) 'b)
  (check-equal? (rhs `(,z . (1 2 3))) '(1 2 3))

  (define (check/walk walk)
    (let* ([l     `((,z . a)  (,x . ,w) (,y . ,z))]
           [l2    `((,y . b)  (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w))]
           [l3    `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w))]
           [l4    `((,x . ,y) (,w . b)  (,z . ,x) (,y . ,z))]
           [v1    `(,x e ,x)]
           [l5    `((,x . b)  (,w . ,v1) (,u . ,w))]
           [l6    `((,y . ,z) (,x . ,y))]

           [bad-l `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z))])

      (define-check (check-walk? v l exp)
        (check-equal? (walk v l) exp))

      (check-walk? z l 'a)                                        ; 14
      (check-walk? y l 'a)                                        ; 15
      (check-walk? x l w)                                         ; 16
      (check-walk? w l w)                                         ; 17

      ;; (check-equal? (walk x bad-l) x); 18

      (check-walk? w l4 'b)                                       ; 19
      (check-walk? x l2 'b)                                       ; 23
      (check-walk? u l2 'b)                                       ; 23
      (check-walk? v l2 'b)                                       ; 23
      (check-walk? w l2 'b)                                       ; 23

      (check-walk? x l3 z)                                        ; 24
      (check-walk? u l3 z)                                        ; 24
      (check-walk? v l3 z)                                        ; 24
      (check-walk? w l3 z)                                        ; 24

      (check-walk? u l5 v1)                                       ; 25

      (check-walk? y `((,x . e)) y)                               ; 30

      (check-walk? y (ext-s y x `((,x . e))) 'e)                  ; 31

      (check-walk? x l6 z)                                        ; 32

      (check-walk? x (ext-s z 'b l6) 'b)                          ; 33
      (check-walk? x (ext-s z w l6) w)                            ; 34
      ))

  (check/walk walk/1)
  (check/walk walk/2)
  (check/walk walk/3)
  (check/walk walk/4)
  (check/walk real/walk)

  (define (check/walk* walk*)
    (define-check (check-walk*? v l exp)
      (check-equal? (walk* v l) exp))

    (check-walk*? x `((,y . (a ,z c)) (,x . ,y) (,z . a)) '(a a c)) ; 44
    (check-walk*? x `((,y . (,z ,w c)) (,x . ,y) (,z . a)) `(a ,w c)) ; 45
    (check-walk*? y `((,y . (,w ,z c)) (,v . b) (,x . ,v) (,z . ,x)) `(,w b c)) ; 46
    )

  (check/walk* walk*/1)
  (check/walk* real/walk*)

  ;; (real/run* (q)                        ; 47 / sidebar -- failing
  ;;            (real/≈ #f q)
  ;;            (project (q)
  ;;                     (real/≈ (not (not q)) q)))

  (check-equal? (let ([r (list w x y)])                           ; 53
                  (walk* r (reify-s r empty-s)))
                '(_.0 _.1 _.2))

  (check-equal? (let ([r (walk* (list x y z) empty-s)])           ; 54
                  (walk* r (reify-s r empty-s)))
                '(_.0 _.1 _.2))

  (let ([r `(,u (,v (,w ,x) ,y) ,z)])                             ; 55
    (check-equal? (walk* r (reify-s r empty-s))
                  '(_.0 (_.1 (_.2 _.3) _.4) _.5)))

  (let ([s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))])
    (let ([r (walk* x s)])
      (check-equal? r `(a ,w c ,w))
      (check-equal? (reify-s r empty-s) `((,w . _.0)))
      (check-equal? (walk* r (reify-s r empty-s))                 ; 56
                    '(a _.0 c _.0))
      (check-equal? (reify r) `(a _.0 c _.0))))                   ; 58

  'done)
