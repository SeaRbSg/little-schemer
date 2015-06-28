#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

;; 6
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

;; 8
[check-equal? (rhs `(,z . b)) 'b]

;; 9
(define w (var 'w))
[check-equal? (rhs `(,z . ,w)) w]

;; 10
[check-equal? (rhs `(,z . (,x e ,y))) `(,x e ,y)]

(define empty-s '())

;; 14
[check-equal? (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a]

;; 15
[check-equal? (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a]

;; 16
[check-equal? (walk x `((,z . a) (,x . ,w) (,y . ,z))) w]

;; 17
[check-equal? (walk w `((,x . ,y) (,z . ,x) (,y . ,z))) w]

;; 18
;; [check-equal? (walk x `((,x . ,y) (,z . ,x) (,y . ,z))) '()]

;; 19
[check-equal? (walk w `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z))) 'b]

;; 23
(define u (var 'u))
(define v (var 'v))
[check-equal? (walk u `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk v `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk w `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]
[check-equal? (walk x `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              'b]

;; 24
[check-equal? (walk u `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk v `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk w `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]
[check-equal? (walk x `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
              z]

;; 25
[check-equal? (walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w))) `(,x e ,x)]

;; 27
(define my-walk
  (lambda (v s)
    (cond
     [(var? v)
      (cond
       [(assq v s) =>
        (lambda (a)
          (my-walk (rhs a) s))]
       [else v])]
     [else v])))

;; 29
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

;; 30
[check-equal? (my-walk y `((,x . e))) y]

;; 31
[check-equal? (my-walk y (ext-s y x `((,x . e)))) 'e]

;; 32
[check-equal? (my-walk x `((,y . ,z) (,x . ,y))) z]

;; 33
[check-equal? (my-walk x (ext-s z 'b `((,y . ,z) (,x . ,y)))) 'b]

;; 34
[check-equal? (my-walk x (ext-s z w `((,y . ,z) (,x . ,y)))) w]

(define my-unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
       [(eq? v w) s]
       [(var? v) (ext-s v w s)]
       [(var? w) (ext-s w v s)]
       [(and (pair? v) (pair? w))
        (cond
         [(my-unify (car v) (car w) s) => 
          (lambda (s)
            (my-unify (cdr v) (cdr w) s))]
         [else #f])]
       [(equal? v w) s]
       [else #f]))))

;; 44
[check-equal? (walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) '(a a c)]

;; 45
[check-equal? (walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(a ,w c)]

;; 46
[check-equal? (walk* y `((,y . (,w ,z c)) (,v . b) (,x . ,v) (,z . ,x))) 
              `(,w b c)]

;; 47
(define my-walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (my-walk* (car v) s)
           (my-walk* (cdr v) s)))
        (else v)))))

;; 52
;; DISCUSSION: if v is walk*-ed already why are we walking it again in
;; the let before doing anything else?

(define my-reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) 
         (ext-s v (reify-name (size-s s)) s)]
        [(pair? v) (my-reify-s (cdr v) 
                            (my-reify-s (car v) s))]
        [else s]))))

;;53
[check-equal? (let ((r `(,w ,x ,y)))
                (walk* r (reify-s r empty-s)))
              '(_.0 _.1 _.2)]

;; 54
[check-equal? (let ((r (walk* `(,x ,y ,z) empty-s)))
                (walk* r (reify-s r empty-s)))
              '(_.0 _.1 _.2)]

;; 55
[check-equal? (let ((r `(,u (,v (,w ,x) ,y) ,x)))
                (walk* r (reify-s r empty-s)))
              '(_.0 (_.1 (_.2 _.3) _.4) _.3)]

;; 56
[check-equal? (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
                (let ((r (walk* x s)))
                  (walk* r (reify-s r empty-s))))
              '(a _.0 c _.0)]

;; 58
(define my-reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

[check-equal? (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
                (reify (walk* x s)))
              '(a _.0 c _.0)]

;; 59
(define ext-s-check
  (lambda (x v s)
    (cond
     [(occurs-check x v s) #f]
     [else (ext-s x v s)])))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
       [(var? v) (eq? v x)]
       [(pair? v)
        (or
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s))]
       [else #f]))))

(define unify-check
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
       [(eq? v w) s]
       [(var? v) (ext-s-check v w s)]
       [(var? w) (ext-s-check w v s)]
       [(and (pair? v) (pair? w))
        (cond
         [(unify-check (car v) (car w) s) =>
          (lambda (s)
            (unify-check (cdr v) (cdr w) s))]
         [else #f])]
       [(equal? v w) s]
       [else #f]))))


;; 62
[check-equal? (run 1 (q) (fresh (x)
                                (== `(,x) x)
                                (== #t q)))
              '(#t)]

;; This is an infinite loop
;; (run 1 (x) (== `(,x) x))

;; This isn't an infinite loop. 
[check-equal? (run 1 (q) (fresh (x)
                                (== `(,x) x)))
              '(_.0)]

;; Nor is this
[check-equal? (run 1 (x) (fresh (x)
                                (== `(,x) x)))
              '(_.0)]

;; Nor is this
[check-equal? (run 1 (q) (== `(,x) x))
              '(_.0)]


;; DISCUSSION: Why is the above true? 

;; 63
[check-equal? (run 1 (q) 
                   (fresh (x y)
                          (== `(,x) y)
                          (== `(,y) x)
                          (== #t q)))
              '(#t)]

;; 64
[check-equal? (run 1 (x) (==-check `(,x) x))
              '()]

;; 65
;; [check-equal? (run 1 (x)
;;                    (fresh (y z)
;;                           (== x z)
;;                           (== `(a b ,z) y)
;;                           (== x y)))
;;               ]


;; 66
[check-equal? (run 1 (x)
                   (fresh (y z)
                          (== x z)
                          (== `(a b ,z) y)
                          (==-check x y)))
              '()]


