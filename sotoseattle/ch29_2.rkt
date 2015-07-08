#lang racket
(require rackunit)
(require racket/trace)

; BUILDING MINI KANREN

(define-syntax λg
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

; A substitution is dictionary.
; A substitution is a set of solutions expressed as a list.
;   This list is an association list, finite and composed of pairs.
;   Each pair has a car/key (a var) and a cdr/value (var or not)
;   example: ((x . 2) (y . 3) (z . x))

; Nomemclature, a list that can be infinite we call stream

; A goal is a function that:
;   - takes in a stream plus other struff and returns a stream
;     with the following characteristics:
;       - each element of the list/stream is a substitution/dict
;       - each dictionary represents a set of possible solutions

; Before anything we need a couple of definitions (unit is the identy function
; A stream of a single dictio, is not a list with a single dictio inside
; but the same dictio. Not (((x . y))), but ((x . y)).
(define-syntax unit (syntax-rules ()
          ((_ a) a)))

; (mzero is just #f, we will use #f as our universal GTFOOT (get the fuck out of there!)
(define-syntax mzero (syntax-rules ()
          ((_) #f)))

; The most basic goal is the success one: given a stream, returns the stream
(define ok
  (λg (s) (unit s)))

; The second most basic is the unsuccessful one: given a stream, return a GTFOOT
(define boom!
  (λg (s) (mzero)))

[check-equal? (ok    '(hola)) '(hola)]
[check-equal? (boom! '(hola)) #f]

; the following building blocks are assumed from ch29

; (define lhs  (lambda (ass) (car ass)))
; (define rhs  (lambda (ass) (cdr ass)))

(define var  (lambda (freshy) (vector freshy)))
(define var? (lambda (anything) (vector? anything)))

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(define walk
  (lambda (v s)
    (cond
      [(var? v) (cond
                  [(assq v s) => (lambda (a) (walk (cdr a) s))]
                  [else v])]
      [else v])))

; (define walk*
;   (lambda (v s)
;     (let ((v (walk v s)))
;       (cond
;         [(var? v) v]
;         [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
;         [else v]))))

(define circular?
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) (eq? v x)]
        [(pair? v) (or (circular? x (car v) s)
                       (circular? x (cdr v) s))]
        [else #f]))))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define ext-s2
  (lambda (x v s)
    (cond
      [(circular? x v s) #f]    ; if x == v already, just fail with #f
      [else (ext-s x v s)])))

(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s2 v w s)]
        [(var? w) (ext-s2 w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify (car v) (car w) s) => (lambda (s) (unify (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s]
        [else #f]))))

; With the above the simplest goal makes all the sense
; == returns a lambda, which takes a dict and extends it if possible
; if not possible (because already exists) returns the same dict
; if not possible (because doesn't make sense) blow up with GTFOOT

(define ==
  (lambda (v w)
    (λg (s)
        (cond
          ; [(unify v w s) => ok]
          ; [else (boom! s)]))))
          [(unify v w s) => (lambda (s) s)]
          [else #f]))))

; This goal is a function that:
;   - takes in a stream plus two things and returns a stream
;     with the following characteristics:
;       - It is the same stream as was passed
;       - it is an extended dictionary
;       - it is #f
; NOOOOOOO, see below and fix it (later)

[check-equal? ((== 0 0)   '()) '()] ; 0 == 0 -> the s remains the same
[check-equal? ((== 0 1)   '()) #f]  ; 0 <> 1 -> boom!
[check-equal? ((== #t #t) '()) '()]
[check-equal? ((== #t #f) '()) #f]

[check-equal? ((== v 0) `())           '((#(v) . 0))]
[check-equal? ((== v 0) `((,v . 0)))   '((#(v) . 0))]
[check-equal? ((== v 1) `((,v . 0)))   #f]
[check-equal? ((== v 0) `((,w . 0)))   '((#(v) . 0) (#(w) . 0))]
[check-equal? ((== v w) `())           '((#(v) . #(w)))]

[check-equal?
  ((== v 0)                 ; let's extend this into this stream...
   `(((,w . 0))             ; stream's dictio #1
     ((,x . 1) (,y . 2))))  ; stream's dictio #2
   '((#(v) . 0)
     ((#(w) . 0))
     ((#(x) . 1) (#(y) . 2)))]

; NOPE, the stream is not of dictios, the stream is the dictio !!!!!!





