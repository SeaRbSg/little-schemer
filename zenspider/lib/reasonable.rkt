#lang racket/base

;; A goal g is a function that maps a substitution s to an ordered
;; sequence s* of zero or more substitutions. (For clarity we notate
;; lambda as λg when creating such a function g.) Because the sequence
;; of substitutions may be infinite, we represent it not as a list but
;; a stream.
;;
;; Streams contain either zero, one, or more substitutions. We use
;; (mzero) to represent the empty stream of substitutions. For
;; example, #%u maps every substitution to (mzero). If a is a
;; substitution, then (unit a) represents the streams containing just
;; a. For instance, #%s maps every substitution to just (unit s). The
;; goal created by an invocation of the ≈ operator maps a substitution
;; s to either (mzero) or to a stream containing a single (possibly
;; extended) substitution, depending on whether that goal fails or
;; succeeds. To represent a stream containing multiple substitutions,
;; we use (choice a f), where a is the first substitution in the
;; stream, and where f is a function of zero arguments. Invoking the
;; function f produces the remainder of the stream, which may or may
;; not be empty. (For clarity, we notate lambda as λf when creating
;; such a function f.)
;;
;; When we use the variable a rather than s for substitutions, it is
;; to emphasize that this representation of streams works for other
;; kinds of data, as long as the datum is never #f or a pair whose cdr
;; is a function--in other words, as long as the three cases above are
;; never represented in overlapping ways. To discriminate among the
;; cases we define the macro case∞.
;;
;; The second case is redundant in this representation: (unit a) can
;; be represented as (choice a (λf '() #f)). We include unit, which
;; avoids building and taking apart pairs and invoking functions
;; because many goals never return multiple substitutions. run
;; converts a stream of substitutions s∞ to a list of values using
;; map∞.

;; Two streams can be merged either by concatenating them using mplus
;; (also known as stream-append) or by interleaving them using
;; mplus-i. The only difference between the definitions mplus and
;; mplus-i lies in the recursive case: mplus-i swaps the two streams;
;; mplus does not.

;; Given a stream s∞ and a goal g, we can feed each value in s∞ to the
;; goal g to get a new stream, then merge all these new streams
;; together using mplus or mplus-i. When using mplus, this operation
;; is called monadic bind, and it is used to implement the fair
;; conjunction all-i. The operators all and all-i are like 'and', since
;; they are short circuiting: the false value short-circuits 'and', and
;; any failed goal short-circuits all and all-i. Also, the let in the
;; third clause of all-aux ensures that (all e), (all-i e), (all e
;; #%s), and (all-i e #%s) are equivalent to e, even if the expression
;; e has no value. The addition of the superfluous second clause
;; allows all-aux expressions to expand to simpler code.

;; To take the disjunction of goals we define cond-e, and to take the
;; fair disjunction we define cond-i. They combine successive
;; question-answer lines using mplus and mplus-i, respectively. Two
;; stranger kinds of disjunction are cond-a and cond-u. When a
;; question g0 succeeds, both cond-a and cond-u skip the remaining
;; lines. However, cond-u chops off every substitution after the first
;; produced by g0, whereas cond-a leaves the stream produced by g0
;; intact.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide %s
         %u
         ≈
         cond-e
         fresh
         none
         run*
         run)

(require (only-in racket/list empty))

(define none empty)

(define var vector)                     ; 9:6

(define var? vector?)                   ; 9:6

(define empty-s '())                    ; 9:13

(define (walk v s)                      ; 9:27
  (let ((rhs cdr))
    (cond ((var? v)
           (cond
             ((assq v s) => (lambda (a) (walk (rhs a) s)))
             (else v)))
          (else v))))

(define (ext-s x v s)                   ; 9:29
  (cons (cons x v) s))

(define (unify v w s)                   ; 9:36
  (if (eq? v w) s                       ; 9:37 optimization
      (let ((v (walk v s))
            (w (walk w s)))
        (cond ((eq? v w) s)
              ((var? v) (ext-s v w s))
              ((var? w) (ext-s w v s))
              ((and (pair? v) (pair? w))
               (cond
                 ((unify (car v) (car w) s) =>
                  (lambda (s) (unify (cdr v) (cdr w) s)))
                 (else #f)))
              ((equal? v w) s)
              (else #f)))))

(define (walk* v s)                     ; 9:47
  (let ((v (walk v s)))
    (cond ((var? v) v)
          ((pair? v) (cons (walk* (car v) s)
                           (walk* (cdr v) s)))
          (else v))))

(define size-s length)

(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

(define (reify-s v s)                   ; 9:52
  (let ((v (walk v s)))
    (cond ((var?  v) (ext-s v (reify-name (size-s s)) s))
          ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
          (else s))))

(define (reify v)
  (walk* v (reify-s v empty-s)))

(define-syntax run                      ; 9 : 6, 13, 47, 58
  (syntax-rules ()
    ((_ ñ (x) g ...)
     (let ((n ñ)
           (x (var 'x)))
       (if (or (not n) (> n 0))
           (map∞ n (lambda (s) (reify (walk* x s)))
                 ((all g ...) empty-s))
           '())))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define-syntax run1
  (syntax-rules ()
    ((_ (x) g ...) (run 1 (x) g ...))))

(define-syntax case∞
  (syntax-rules ()
    ((_ e zero ((â) one) ((a f) many))
     (let ((a∞ e))
       (cond ((not a∞) zero)
             ((not (and (pair? a∞) (procedure? (cdr a∞))))
              (let ((â a∞))
                one))
             (else (let ((a (car a∞))
                         (f (cdr a∞)))
                     many)))))))


(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define (map∞ n p a∞)
  (case∞ a∞
         '()
         ((a) (cons (p a) '()))
         ((a f) (cons (p a) (cond
                              ((not n) (map∞ n p (f)))
                              ((> n 1) (map∞ (- n 1) p (f)))
                              (else '()))))))

(define-syntax λg (syntax-rules () ((_ a c ...) (lambda a c ...))))
(define-syntax λf (syntax-rules () ((_ a c ...) (lambda a c ...))))

(define #%s (λg (s) (unit s)))
(define #%u (λg (s) (mzero)))
(define %s #%s)                         ; TODO: cleanup
(define %u #%u)                         ; TODO: cleanup


;; (set-sharp-read-syntax! #\s (lambda (port) '#%s))
;; (set-sharp-read-syntax! #\u (lambda (port) '#%u))

(define (≈ v w)
  (λg (s)
      (cond
        ((unify v w s) => #%s)
        (else (#%u s)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g ...)
     (λg (s)
         (let ((x (var 'x)) ...)
           ((all g ...) s))))))

(define-syntax all    (syntax-rules () ((_ g ...) (all-aux bind  g ...))))
(define-syntax all-i  (syntax-rules () ((_ g ...) (all-aux bind-i g ...))))

(define-syntax all-aux
  (syntax-rules ()
    ((_ bnd)          #%s)
    ((_ bnd g)        g)
    ((_ bnd g0 g ...) (let ((g^ g0))
                        (λg (s)
                            (bnd (g^ s)
                                 (λg (s) ((all-aux bnd g ...) s))))))))

(define-syntax cond-e (syntax-rules () ((_ c ...) (cond-aux if-e c ...))))
(define-syntax cond-i (syntax-rules () ((_ c ...) (cond-aux if-i c ...))))
(define-syntax cond-a (syntax-rules () ((_ c ...) (cond-aux if-a c ...))))
(define-syntax cond-u (syntax-rules () ((_ c ...) (cond-aux if-u c ...))))

(define-syntax cond-aux
  (syntax-rules (else)
    ((_ ifer) #%u)
    ((_ ifer (else g ...))     (all g ...))
    ((_ ifer (g ...))          (all g ...))
    ((_ ifer (g0 g ...) c ...) (ifer g0 (all g ...) (cond-aux ifer c ...)))))

(define (mplus a∞ f)
  (case∞ a∞
         (f)
         ((a)    (choice a f))
         ((a f0) (choice a (λf () (mplus   (f0) f))))))

(define (mplus-i a∞ f)
  (case∞ a∞
         (f)
         ((a)    (choice a f))
         ((a f0) (choice a (λf () (mplus-i (f) f0))))))

(define (bind a∞ g)
  (case∞ a∞
         (mzero)
         ((a)   (g a))
         ((a f) (mplus   (g a) (λf () (bind   (f) g))))))

(define (bind-i a∞ g)
  (case∞ a∞
         (mzero)
         ((a)   (g a))
         ((a f) (mplus-i (g a) (λf () (bind-i (f) g))))))

(define-syntax if-e
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λg (s) (mplus ((all g0 g1) s) (λf () (g2 s)))))))

(define-syntax if-i
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λg (s) (mplus-i ((all g0 g1) s) (λf () (g2 s)))))))

(define-syntax if-a
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λg (s)
         (let ((s∞ (g0 s)))
           (case∞ s∞
                  (g2 s)
                  ((s) (g1 s))
                  ((s f) (bind s∞ g1))))))))

(define-syntax if-u
  (syntax-rules ()
    ((_ g0 g1 g2)
     (λg (s)
         (let ((s∞ (g0 s)))
           (case∞ s∞
                  (g2 s)
                  ((s) (g1 s))
                  ((s f) (g1 s))))))))

(module+ test
  (require rackunit)

  (provide (all-defined-out))

  (define-syntax check-run*
    (syntax-rules (=>)
      [(_ (vars ...) conds ... => exp) (check-equal? (run* (vars ...) conds ...)
                                                     exp)]))
  (define-syntax check-run1
    (syntax-rules (=>)
      [(_ (vars ...) conds ... => exp) (check-run 1 (vars ...) conds ...
                                                  => exp)]))

  (define-syntax check-run
    (syntax-rules (=>)
      [(_ n (vars ...) conds ... => exp) (check-equal? (run n (vars ...) conds ...)
                                                       exp)])))
