#lang racket
(require rackunit)

;; When creating a goal function
(define-syntax λG
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

;; When creating a function to produce more stream
(define-syntax λF
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run
  (syntax-rules ()
    ((_ nˆ (x) g ...)
     (let ((n nˆ) (x (var x)))
       (if (or (not n) (> n 0))
           (map∞ n
                 (lambda (s)
                   (reify (walk* x s)))
                 ((all g ...) empty-s))
           '())))))

(define-syntax case∞
  (syntax-rules ()
    ((_ e on-zero ((aˆ) on-one) ((a f) on-choice))
        (let ((a∞ e))
          (cond
           [(not a∞) on-zero]
           [(not (and
                  (pair? a∞)
                  (procedure? (cdr a∞))))
            (let ((aˆ a∞))
              on-one)]
           [else (let ((a (car a∞)) (f (cdr a∞)))
                   on-choice)])))))

;; mzero is just a funny name for false
(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

[check-equal? (mzero) #f]

;; unit is just the identity function
(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

[check-equal? (unit 'a) 'a]

;; Choice is a value (substitution) and a function
(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

[check-equal? (choice 'a 'b) '(a . b)]


;; Map∞ takes three args
;;  n - the number of values to return
;;  p - a predicate?!?!                ;; Double check this
;;  a - a stream
;; if a is empty it returns empty
;; if a has a single value it conses p of a onto the empty list
;; if a has a value and a function
;;   it cons p of a onto
;;      if n is missing (so it is a run*) the map∞ of n p executed f
;;      if n is greater than one it decrements n and does a map∞
;;      empty list if n is 1 or less 
(define map∞
  (lambda (n p a∞)
    (case∞ a∞
           '()
           ((a) 
            (cons (p a) '()))
           ((a f)
            (cons (p a)
                  (cond
                   ((not n) (map∞ n p (f)))
                   ((> n 1) (map∞ (- n 1) p (f)))
                   (else '())))))))

;; TODO I don't understand the last arg
;;[check-equal? (map∞ #f (lambda (x) x) ()) ]

;; satisfactory is just the identity function. A pass through.
(define s# (λG (s) (unit s)))

;; unsatisfied always returns an empty list
(define u# (λG (s) (mzero)))

;; == creates a function that tries to unify v & w with a given
;; substitution list. Whatever substitution it finds is passed through
;; unit. Otherwise mzero (#f)
(define ==
  (lambda (v w)
    (λG (s)
        (cond
         [(unify v w s) => s#]
         [else (u# s)]))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g ...)
     (λG (s)
         (let ((x (var 'x)) ...)
           ((all g ...) s))))))


(define-syntax conde
  (syntax-rules ()
    ((_ c ...) (cond-aux ife c ...))))

(define-syntax all
  (syntax-rules ()
    ((_ g ...) (all-aux bind g ...))))

(define-syntax alli
  (syntax-rules ()
    ((_ g ...) (all-aux bindi g ...))))

(define-syntax condi
  (syntax-rules ()
    ((_ c ...) (cond-aux ifi c ...))))

(define-syntax conda
  (syntax-rules ()
    ((_ c ...) (cond-aux ifa c ...))))

(define-syntax condu
  (syntax-rules ()
    ((_ c ...) (cond-aux ifu c ...))))


(define mplus
  (lambda (a∞ f)
    (case∞ a∞
      (f) 
      ((a) (choice a f))
      ((a f0) (choice a 
                (λF () (mplus (f0) f)))))))

(define bind
  (lambda (a∞ g)
    (case∞ a∞
           (mzero)
           ((a) (g a))
           ((a f) (mplus (g a) (λF () (bind (f) g)))))))

(define mplusi
  (lambda (a∞ f)
    (case∞ a∞
           (f)
           ((a) (choice a f))
           ((a f0) (choice a (λF () (mplusi (f) f0)))))))

(define bindi
  (lambda (a∞ g)
    (case∞ a∞
           (mzero)
           ((a) (g a))
           ((a f) (mplusi (g a) (λF () (bindi (f) g)))))))

(define-syntax cond-aux
  (syntax-rules (else)
    ((_ ifer) u#)
    ((_ ifer (else g ...)) (all g ...))
    ((_ ifer (g ...)) (all g ...))
    ((_ ifer (g0 g ...) c ...)
     (ifer g0
           (all g ...)
           (cond-aux ifer c ...)))))

(define-syntax ife
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λG (s)
         (mplus ((all g0 g1) s) (λF () (g2 s)))))))

(define-syntax ifa
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λG (s)
         (let ((s∞ (g0 s)))
           (case∞ s∞
                  (g2 s)
                  ((s) (g1 s))
                  ((s f) (bind s∞ g1))))))))

(define-syntax all-aux
  (syntax-rules ()
    ((_ bnd) s#)
    ((_ bnd g) g)
    ((_ bnd g0 g ...)
     (let ((gˆ g0))
       (λG (s)
           (bnd (gˆ s)
                (λG (s) ((all-aux bnd g ...) s))))))))

(define-syntax ifi
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λG (s)
         (mplusi ((all g0 g1) s) (λF () (g2 s)))))))

(define-syntax ifu
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λG (s)
         (let ((s∞ (g0 s)))
           (case∞ s∞
                  (g2 s)
                  ((s) (g1 s))
                  ((s f) (g1 s))))))))


(define-syntax rhs
  (syntax-rules ()
    ((rhs p) (cdr p))))

(define-syntax lhs
  (syntax-rules ()
    ((lhs p) (car p))))

(define-syntax var
  (syntax-rules ()
    ((var w) (vector w))))

(define-syntax var?
  (syntax-rules ()
    ((var? w) (vector? w))))

(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
       [(eq? v w) s]
       [(var? v) (ext-s v w s)]
       [(var? w) (ext-s w v s)]
       [(and (pair? v) (pair? w))
        (cond
         [(unify (car v) (car w) s) => 
          (lambda (s)
            (unify (cdr v) (cdr w) s))]
         [else #f])]
       [(equal? v w) s]
       [else #f]))))

(define walk
  (lambda (v s)
    (cond
     [(var? v)
      (cond
       [(assq v s) =>
        (lambda (a)
          (walk (rhs a) s))]
       [else v])]
     [else v])))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define empty-s '())
