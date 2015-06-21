#lang racket
(require "basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

(define var  (lambda (freshy) (vector freshy)))
; A vector is a fixed-length array of arbitrary values.
; list   '(1 2 3)
; vector #(1 2 3)

(define var? (lambda (anything) (vector? anything)))
; Unlike a list, a vector supports constant-time access and update of its elements.

; we can define our own fresh variables as single-element vectors (kindalists).
; so a var/vector u creates a fresh variable u that stores the value 'u
(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

; A variable is fresh when it has not yet been binded/associated
; A fresh variable, has no associated value, so what happens when we query it about its association?
; it will return a weird string "_.#{n}" <== the result of reify-name
; meaning that the variable has been reified, as a way to show that it is fresh
; and therefore not associated to anything (empty, virgin, waiting for its better-half)

[check-equal? (run* (x) s#) '(_.0)]

; LAW OF FRESH: if x is fresh, x can be associated, == will succeed

; (z . a) is a malformed list, not proper, it is a pair
; for us it will be  an ASSOCIATION between a variable [z] and something else [a]
; for which z must be a var and a can be anything (var or value)
; we also rename its car/cdr to lhs/rhs

(define lhs  (lambda (ass) (car ass)))
(define rhs  (lambda (ass) (cdr ass)))

; Our definition works for both proper and non-proper lists
; in terms of searching/storing is like a hash:
;   (lhs assoc => key [var]
;   (rsh assoc => value
; An assoc cannot be self-referencing (x . x) because we say so

; A substitution is a list of assocs
; This chapter is about Unification. From Wikipedia, Unification is an algorithmic process of solving
; equations between symbolic expressions. A solution of a unification problem is a substitution,
; a mapping assigning a symbolic value to each variable of the problem's expressions.
; For example lets solve: (cons x (cons x '()))  = (cons 2 (cons y '()))
; the solution is the Substitution { x->2, y->2 } or '((x . 2) (y . 2))

[check-equal?
  (run* (q)
    (fresh (x y res)
      (conso x x res)
      (conso 2 y res)
      (== `((x . ,x) (y . ,y)) q) ; don't know how else to build s ad-hoc!
      ))
  '(((x . 2) (y . 2)))]

; So run is an algorithmic solver of symbolic equations that finds the associations to the
; symbols (variables).

(define empty-s '()) ; empty substitution, a subst with no assocs inside

; # 21 yes, because unify if successful will extend s with a pair (var . val)

; # 27
(define walk
  (lambda (v s)
    (cond
      [(var? v) (cond                                              ; [2]
                  [(assq v s) => (lambda (a) (walk (rhs a) s))]    ; [4]
                  [else v])]                                       ; [3]
      [else v])))                                                  ; [1]

; [1] If the key is not even a var, since all keys must be vars, just return it.
; [2] If it is a var...
; [4] ... search for the key value pair in s (assq)
          [check-equal? (assq 3 (list (list 1 2) (list 3 4) (list 5 6))) '(3 4)]
;         if it finds it, recur the walk but now using as key the rhs found.
;     The final val/var is returned
; [3] ... and if it doesn't find it, return itself

; Consider that:
;   - if walk returns itself => it has failed the search
;   - '=>' pipes through. So (assq v s) -> (key value) and (rhs (key val)) -> val

; # 27 bis -- the following is longer but easier to follow
(define wander
  (lambda (ass s)
    (my_walk (rhs ass) s)))

(define my_walk
  (lambda (v s)
    (cond
      [(var? v) (let ((ass_found (assq v s)))
                  (cond
                    [ass_found (wander ass_found s)]
                    [else v]))]
      [else v])))

[check-equal? (my_walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a]
[check-equal? (my_walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a]
[check-equal? (my_walk x `((,z . a) (,x . ,w) (,y . ,z)))  w]
[check-equal? (my_walk w `((,z . a) (,x . ,w) (,y . ,z)))  w]

; # 29
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

; (walk x (ext-s x y `((,z . ,x) (,y . ,z)))) <=== moebius path
[check-equal? (walk y (ext-s x 'pepe `((,z . ,x) (,y . ,z)))) 'pepe]

; # 35 Consider that other procs like assq also return #f if they fail
; this behavior is very useful when used inside cond because it allow rerouting
; so unify will succeed (and modify the s) or fail (and return #f)

; Remember that a s is a list of assocs of variables such that solves the set of equations
; unifying means modifying the solution by reconnecting vars to vals

(define unify
  (lambda (v w s)
    (let ((v (walk v s))           ; get me the end value
          (w (walk w s)))          ; the same for w
      (cond
        [(eq? v w) s]              ; if v & w are the exact same thing, they are already unified, no need to change s
        [(var? v) (ext-s v w s)]   ; the key must be a var => create a new assoc, because it was not in s => extend
        [(var? w) (ext-s w v s)]   ; the same if v was not var, but w is
        [(and (pair? v) (pair? w)) ; if they are both lists
         (cond
           [(unify (car v) (car w) s) => (lambda (s) (unify (cdr v) (cdr w) s))] ; [see below]
           [else #f])]
        [(equal? v w) s]           ; not being the same thing, yet identical (loose ==)
        [else #f]))))

; We unify the cars, and pass the resulting s to keep unifying recursively
; at the end we'll have the final s modified as many times as elements/recursions found
; Consider that if the lists are of unequal size, there is no 1-1 mapping, we cannot unify and it fails
; Here the ability of unify to return #f is clear. It allows to jump to the else statement.
; # 37 & 38 are very good insights
; # 44
[check-equal? (walk x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a ,z c)]
; this is a superficial walk
; walk* would try to find the eol of the freshy returned in the solution (a a c)

[check-equal? (walk x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(,z ,w c)]
; walk* would try to find the eol of the freshy returned in the solution (a ,w c)

(define my_walk*
  (lambda (v s)
    (cond
      [(var? v) (cond
                  [(assq v s) => (lambda (a) (my_walk* (rhs a) s))]
                  [else v])]
      [(pair? v) (cons (my_walk* (car v) s) (my_walk* (cdr v) s))] ; added
      [else v])))

[check-equal? (my_walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a a c)]
[check-equal? (my_walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(a ,w c)]

; we can simplify reusing walk
(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) v]
        [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
        [else v]))))

[check-equal? (walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a))) `(a a c)]
[check-equal? (walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a))) `(a ,w c)]

; A variable that has been walked and ends being a var ==> is fresh always!!!
; If we always start with empty-s, and we add assocs to it with unify:
;   then all values that are vars, will have to be fresh
;   because if not, they would be associated, and would show up as key in s
; So if the result of walk is a var ==> it is a fresh one

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

[check-equal? (reify-name 67535) '_.67535] ; _.67535 is a symbol, not a string/atom/etc

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) (ext-s v (reify-name (length s)) s)]
        [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
        [else s]))))

; in the let we walk s looking for the resultant of v ==>
; 'walked v' will be v (itself), a value, or a fresh var
; if walked-v is a var (fresh), add assoc (v . _.n) to s. Remember _.n is a symbol
;   it only means that v is fresh and unbound
; if walked-v is a pair, reify the car into the s, and using that s
;   recurr reifying the cdr
; since we add the assocs from the left, the vars show reversed (right to left)
; but the enumeration is from left to right (as we grow s reifying cars)

[check-equal?
  (reify-s x empty-s) ; x is var => insert (x . _.0) into empty-s ()
  `((,x . _.0))]

[check-equal?
  (reify-s (list x y) empty-s) ; now pair => same but from bottom up => first y then x
  `((,y . _.1) (,x . _.0))]

[check-equal?
  (reify-s (list w x y) empty-s) ; check the numbering !!
  `((,y . _.2) (,x . _.1) (,w . _.0))]

; Given a list of variables and an empty-s, by reifying we initialize all fresh
; variables to their symbols. We create a list of assocs ((x . _.n)...)

[check-equal?
  (let ((r (list w x y)))
    (walk* r (reify-s r empty-s))) ; (walk (w x y) ((y . _.2) (x . _.1) (w . _.0)))
  '(_.0 _.1 _.2)]

[check-equal?
  (let ((r (walk* (list x y z) empty-s))) ; (x y z) HA HA, its the same
    (walk* r (reify-s r empty-s)))
  '(_.0 _.1 _.2)]

[check-equal?
  (let ((r `(,u (,v (,w ,x) ,y) ,x))) ; x is repeated and already reified
    (walk* r (reify-s r empty-s)))    ; _0 _1 _2 _3 _4 _3
  '(_.0 (_.1 (_.2 _.3) _.4) _.3)]

; Before, walking an s, gave me the corresponding fresh var/vals
; [check-equal? (my_walk* x `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))) `(a #(w) c #(w))]

; Walking an initialized (reified) s, instead of the vars it returns its symbols
[check-equal?
  (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
    (let ((r (walk* x s)))                             ; a ,w c ,w
      (walk* r (reify-s r empty-s))))                  ; a _.0 c _.0
  '(a _.0 c _.0)]

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

; Now we can define how to reify a var, which is nothing but:
; - reinitialize the end value vars to symbols
; - make sure that the end vars are fresh and unbound/unassociated
[check-equal? (reify x) '_.0]

; so? why do we need and where do we use reify?
; because in (run we do a (reify (walk* x s)) [see mk.rkt]
; meaning we get the end-resultant of x, where all vars are fresh
; and then we substitute those fresh variables with their reified names (symbols)
; We'll study this further when we go through mk.rkt

; I think that the following implements frame 38 for an updated version of unify

(define occurs✓
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        [(var? v) (eq? v x)]
        [(pair? v) (or
                     (occurs✓ x (car v) s)
                     (occurs✓ x (cdr v) s))]
        [else #f]))))

(define ext-s✓
  (lambda (x v s)
    (cond
      [(occurs✓ x v s) #f]    ; if x == v already, just fail with #f
      [else (ext-s x v s)])))

(define unify✓
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s✓ v w s)]
        [(var? w) (ext-s✓ w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify✓ (car v) (car w) s) => (lambda (s) (unify✓ (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s]
        [else #f]))))

; from mk.rkt and zenspider's lib/reasonable.rkt

; A goal g is a function that takes a substitution s and returns a stream of substitutions.
; (For clarity we notate lambda as λg when creating such a function g.)
; Because the sequence of substitutions may be infinite, we represent it not as a list but a stream.
; Streams contain either zero, one, or more substitutions. Can be finite or infinite, but
; we generalized from now on to infinite.

; DOUBT: A stream of substitutions, is it the same as an infinite s, with assocs and
; deeply nested assocs (substitutions themselves) inside?

(define-syntax λg
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

; We use (mzero) to represent the empty stream of substitutions '().
; codewise mzero is the same as #f
; (define-syntax mzero
;   (syntax-rules ()
;     ((_) #f)))

; #u is a goal, takes as input an s, and returns mzero.
; (define fail (λg (s) (mzero)))

; If a is a substitution, then (unit a) represents the stream containing just a.
; A finite stream with that single substitution.
;
; #s is a goal that takes as input s, and returns a stream with only s (unit s).
; (define succeed (λg (s) (unit s)))

; == creates a goal, a function that takes an s, and tries to extend it as seen above
; if it succeeds, the new s (which we have seen can be the same input s) is given
; as input to #s/succeed and returns a stream with just s.
; if it fails to unify, it invokes the goal #u/fail and return the mzero stream

(define my_==
  (lambda (v w)
    (λg (s)
      (cond
        ((unify v w s) => s#) ; ((unify v w s) => succeed)
        (else (u# s))))))     ; (else (fail s))))))

; run converts a stream of substitutions a-inf to a list of values using map-inf.

(define-syntax my_run
  (syntax-rules ()
    ((_ n^ (x) g ...)
     (let ((n n^) (x (var 'x)))             ; <== creates the query variable as fresh
       (if (or (not n) (> n 0))
         (map-inf n
           (lambda (s) (reify (walk* x s)))
           ((all g ...) empty-s))           ; <== the result of this is the infinite stream
         '())))))

; the key seems to be in map-inf (map infinite stream), which takes three inputs:
; n => the number of solutions to cap at
; lambda => a procedure to reify whatever the query var is associated with
; a-inf => ((all g ...) ()) the stream of substitutions

; Lets see with an example
; (run* (q)
;   (== #t q))
; we have a single call (== which creates a goal/λg that outputs a stream
; that stream (unitary in this case) is mapped to a list of values by run*

; check how map-inf uses the reify as it goes down the stream
; (define map-inf
;   (lambda (n p a-inf)
;     (case-inf a-inf
;       '()
;       ((a)
;        (cons (p a) '())) ; <== (p a) :: (reify (walk* q a)) :: (reify #t) :: #t
;       ((a f)
;        (cons (p a)
;          (cond
;            ((not n) (map-inf n p (f)))
;            ((> n 1) (map-inf (- n 1) p (f)))
;            (else '())))))))

; (run 1 (x) (== `(,x) x))    ; runs for ever :: circular
; (walk* x `((,x . (,x))))    ; runs for ever :: circular
; the s created is ((x . (x)) ==> which shows when run as ((x x)) [don't get confused]

[check-equal?
  (run 1 (q)
    (fresh (x)
      (== (list x) x) ; we know this is circular, but q doesn't care about x
      (== #t q)))
  '(#t)]

; run will apply the proc to the stream and (reify whatever is associated to q
; which is #t, x is not considered and the circular loop avoided

[check-equal?
  (run 1 (q)
    (fresh (x y)
      (== (list x) y)
      (== (list y) x)
      (== #t q)))
  '(#t)]

(define ==✓
  (lambda (v w)
    (λg (s)
        (cond
          [(unify✓ v w s) => s#]
          [else (u# s)]))))

[check-equal?
  (run 1 (x) (==✓ (list x) x))
  '()]

; unify will add the assoc (x x)...
[check-equal? (unify `(,x) x '()) '((#(x) #(x)))]
; which goes circular ... but unify✓ will plainly fail
[check-equal? (unify✓ `(,x) x '()) '#f]
; because in ext-s✓ the condition (occurs✓ x x s) will trigger the #f

; (run 1 (x)
;   (fresh (y z)
;     (== x z)
;     (== `(a b ,z) y)
;     (== x y)))         ; circular too

(run 1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (==✓ x y)))          ; in the same way the circularity is averted

; when it fails, what is the s?
; first unification  -> s: ((x . z))                    add (x . z) to ()
; second unification -> s: ((y . `(a b ,z)) (x . z))    add (y . (a b z)) to ((x . z))
; third unification  -> fails and the s is unchanged

; the final frames have been explained previously
