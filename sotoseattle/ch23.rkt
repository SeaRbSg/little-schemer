#lang racket
(require "./basic_defs.rkt")
(require "lib/shared.rkt")
(require rackunit)
(require racket/trace)
(require "../lib/mk.rkt")

; # 5
(define listo
  (lambda (l)
    (conde
     ((nullo l) s#)
     ((pairo l) (fresh (d)
                       (cdro l d)
                       (listo d)))
     (else u#))))

; # 7 - 8 - 9
[check-equal?
 (run* (x) (listo `(a b ,x d)))
 '(_.0)]

; # 10 - 11 - 12
[check-equal?
 (run 1 (x) (listo `(a b . ,x)))
 '(())]

; # 14 - 15
[check-equal?
 (run 5 (x) (listo `(a . ,x)))
 '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))]

; FIRST APPROACH: pure logic
; he x that makes the listo succeed can be any possible list (including the empty one).

; SECOND APPROACH: walk the code
; Keep in mind that: (run* (q) (pairo q)) '(_.0 . _.1) is a solution for q, and that
; (run* (x) (nullo x)) '() is a sol for x

; We start with l being ('a . x)
;   [nullo ?] Nope, it is not unificable with '(), we go to next conde
;    [pairo ?] Yeap, it is a pairo (because it can be broken in car and cdr)
;      we bind x with d
;      we do listo of x ==> Recurr ==> We start with l being x
;        [nullo ?] YES, succeeds because (nullo x) '() is sol of x => FIRST SOL '()
;        [pairo ?] YES, succeeds because (pairo x) '(_.0 . _.1) is sol for x
;          we bind _.1 to d
;          we do listo of _.1 ==> Recurr == We start with l being _.1
;            [nullo ?] YES, get out of recursion with _.1 == '() ==> SECOND SOL '(_.0)
;            [pairo ?] YES, '(_.2 . _.3) is sol of _.1
;              we bind _.3 to d
;              we do listo of _.3 ==> Recurr == We start with l being _.3
;                ... ad infinitum and beyond ...

; # 16
(define lol?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(list? (car l)) (lol? (cdr l))]
      [else #f])))

; # 17 - 18 - 19
(define lolailo
  (lambda (l)
    (conde
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (listo a))
      (fresh (b)
             (cdro l b)
             (lolailo b))))))

; # 20
[check-equal?
 (run 1 (l) (lolailo l))
 '(())]

; # 21
[check-equal?
 (run* (q)
       (fresh (x y)
              (lolailo `((a b) (,x c) (d ,y)))
              (== #t q)))
 '(#t)]

; # 22
[check-equal?
 (run 1 (q)
      (fresh (x)
             (lolailo `((a b) . ,x))
             (== #t q)))
 '(#t)]

; for the same reasons as before in the long explanation. x nullo succeeds and is the first conde

; # 23 - 24 - 25 (simplified)
[check-equal?
 (run 3 (q)
      (lolailo `((a) . ,q)))
 '(() (()) (() ()))]

; keep in mind:
; (run* (x) (fresh (k) (caro (cons '(a) x) k)))
; (run* (x) (fresh (k) (caro x k)))
; (run* (x) (fresh (k) (cdro x k)))

; let's walk the lolailo code again:
; l is (('a) . x)
;   [nullo ?] nope
;   [caro] extract a as '(a), is it a listo? YES, but go to next
;   [cdro] extract b as x, is x a lolailo?, let's see:
;     l is x, we get in conde (OR conditions)
;       Conde 1: [nullo] YES, x is '() => that overall lolailo succeeds => first solution of q is '()
;       Conde 2: [caro] a is a freshy _.0, wich is a listo (all possible listos), &&
;                [cdro] b is a freshy , is it a lolailo?
;                l is '(_.0), we get in conde (OR conditions)
;                  Conde 1: [nullo] Yes, (nullo of freshy) succeeds for freshy being '() => 2nd solution is '(())
;                     Of all the possible listos of before, this one collapses the solutions domain
;                     to make both goals succeed
;                  Conde 2: [caro] yes
;                           [cdro] b is a freshy again ==> (run* (x) (fresh (k) (cdro x k))) => (_.0 . _.1)
;                           l is '(_.0), we get in conde (OR conditions)
;                             Conde 1: [nullo] Yes, pull out, again of all possible listos => '(() ())
; so ad infinitum we find hits that succeed and add '() to the list of solutions

; # 26 to 36
(define twinso_0
  (lambda (s)
    (fresh (x y)
           (conso x y s)
           (conso x '() y))))

[check-equal?
 (run* (q)
       (twinso_0 '(tofu tofu))
       (== #t q))
 '(#t)]

[check-equal?
 (run* (z)
       (twinso_0 `(,z tofu)))
 '(tofu)]

(define twinso
  (lambda (s)
    (fresh (x y)
           (== `(,x ,x) s)))) ; <==== coool

[check-equal?
 (run* (z)
       (twinso `(,z tofu)))
 '(tofu)]

; # 37
(define loto
  (lambda (l)
    (conde
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (twinso a))
      (fresh (b)
             (cdro l b)
             (loto b))))))

[check-equal?
 (run 1 (q) (loto q))
 '(())]                ; obvious, the nullo succeeds for empty list

[check-equal?
 (run 2 (q) (loto q))
 '(() ((_.0 _.0)))]

; caro of q => (_.0 . _.1) && twinso means sol is (_.0 _.0) << now proper >>
; (run* (q) (fresh (x) (caro q x) (twinso q)))
; so so far, l has a first element with (_.0 _.0), we do not know the second ((_.0 _.0) . ??)

; cdro of q => (_.0 . _.1) and loto of that means that the first conde it finds is the nullo
; but the (nullo (_.0 . _.1)) fails with '() => pulls out into ((_.0 _.0) . ??):
; ((_.0 _.0) . ()) ==> ((_.0 _.0)) <====== OUR SECOND SOLUTION
;
; since it was a conde (OR conds), lets go back to the car/twinso && cdr/loto of (_.0 . _.1)
; the caro/twinso is clear again ==> (fresh_i . fresh_j) == twinsoed into (fresh_i fresh_i)
; the cdro works similarly, and the recurring lotto will fail again in the nullo with '(), so
; ((_.0 _.0) . ??) ==> ((_.0 _.0) . ((fresh_i fresh_i)  '())) =>
; ((_.0 _.0) . ((fresh_i fresh_i))) ==> ((_.0 _.0) (fresh_i fresh_i)) ==>
; ((_.0 _.0) (_.1 _.1)) ==> OUR THIRD SOLUTION, and on and on

; # 38 - 39 - 40 - 41
[check-equal?
 (run 1 (z)
      (loto `((g g) . ,z)))
 '(())]

; # 42 to 47
[check-equal?
 (run 3 (out)
      (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (loto out)))
 '(((g g) (e e) (_.0 _.0))
   ((g g) (e e) (_.0 _.0) (_.1 _.1))
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]

; # 48
(define listofo
  (lambda (predo l)
    (conde
     ((nullo l))
     ((fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))))))

; # 49
[check-equal?
 (run 3 (out)
      (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (listofo twinso out)))
 '(((g g) (e e) (_.0 _.0))
   ((g g) (e e) (_.0 _.0) (_.1 _.1))
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))]

; we are just passing the twinso as argument predo

; # 50
(define bono-loto
  (lambda (l)
    (listofo twinso l)))

; # 51 to 57
(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) #t]
      [else (member? x (cdr l))])))

[check-true (member? 'olive '(virgin olive oil))]

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membrillo
  (lambda (x l)
    (conde
     ;((nullo? l) u#)           ; unnecessary like the else statements because it fails
     ((eq-caro l x))
     ((fresh (b)
             (cdro l b)
             (membrillo x b))))))

[check-equal?
 (run* (q)
       (membrillo 'olive '(virgin olive oil))
       (== #t q))
 '(#t)]

[check-equal?
 (run* (q)
       (membrillo 'olive '(virgin olive olive oil))
       (== #t q))
 '(#t #t)]

[check-equal? (run* (y) (membrillo y '())) '()] ; because (caro/cdro '() x) both fail
                                                ; (run* (x) (caro '() x) (== x #t))

; # 58 to 64
[check-equal? (run 1 (y) (membrillo y '(hummus with pita))) '(hummus)]
[check-equal? (run* (y) (membrillo y '(a b c))) '(a b c)]

; membrillo gives back a list of solutions where every solution satisfies that it is a member of the given list
; the caro conde finds solutions of x
; the cdro conde, being a different conde (OR), refreshes and as it recurrs finds NEW solutions of x
;   that are added to the list of solution ==> becoming in order, the initial l

; # 65
(define identity
  (lambda (l)
    (run* (q)
          (membrillo q l))))

; # 66 to 68
[check-equal?
 (run* (x)
       (membrillo 'e `(pasta ,x fagioli)))
 '(e)]

; interestingly enough a solution for x is 'e because it will succeed only at the eq-caro '(,x) 'e
; and caro is about unifying, about checking if x and 'e can be unified,
; independently of the order (== x 'e) === (== 'e x)

; # 69 to 72 (for the following thread slowly...)
[check-equal?
 (run 1 (x) (membrillo 'e `(e)))    ; ok, logical. membrillo succeeds but x is unknown
 '(_.0)]

[check-equal?
 (run 1 (x) (membrillo 'e `(,x)))   ; ok, logical too, now x has a solution
 '(e)]

[check-equal?
 (run* (x) (membrillo 'e `(e ,x)))  ; now it makes sense
 '(_.0 e)]

[check-equal?
 (run* (x) (membrillo 'e `(pasta ,x e fartura)))  ; got it
 '(e _.0)]

[check-equal?
 (run* (r)
       (fresh (x y)
              (membrillo 'e `(pasta ,x fagioli ,y))
              (== `(,x ,y) r)))
 '((e _.0) (_.0 e))]

; the important thing is that as we change condes, as we have the caro succeed, the x is refreshed!

; # 73 to 77
[check-equal?
 (run 4 (l)
      (membrillo 'tofu l))
 '((tofu . _.0)
   (_.0 tofu . _.1)
   (_.0 _.1 tofu . _.2)
   (_.0 _.1 _.2 tofu . _.3))]

; conde 1: (eq-car l 'tofu) => l has a car of tofu
; conde 2: (membrillo 'tofu (cdro l)) => recur and since we refresh the caro of the cdro is now tofu
;          and the previous caro of l stays fresh

; # 78 & 79
(define pimentero ; pmembero
  (lambda (x l)
    (conde
     ((eq-caro l x) (cdro l '())) ; the only change is this addition, to pick the last element!!
     ((fresh (d)                  ; because we add a && conde of the cdro being '()
            (cdro l d)
            (pimentero x d))))))

; for comparison purposes
; (define membero as membrillo
;   (lambda (x l)
;     (conde
;      ((eq-caro l x))
;      ((fresh (b)
;              (cdro l b)
;              (membero x b))))))

; # 80
[check-equal?
 (run 4 (l)
      (pimentero 'tofu l))
 '((tofu) (_.0 tofu) (_.0 _.1 tofu) (_.0 _.1 _.2 tofu))]

; # 81 & 82
[check-equal?
 (run* (q)
       (pimentero 'tofu `(tofu tofu))
       (== #t q))
 '(#t)]

; why not '(#t #t)? Because pimentero succeeds with a single solution.

; # 83
(define porrompompero ; yet another pmembero (aka membrillo)
  (lambda (x l)
    (conde
     ((eq-caro l x) (cdro l '()))   ; as before, to pick the last
     ((eq-caro l x))                ; picks the first of l (in recursion too)
     ((fresh (d)
             (cdro l d)
             (porrompompero x d))))))

; # 84 & 85
[check-equal?
 (run* (q)
       (porrompompero 'tofu '(a b tofu d tofu))
       (== #t q))
 '(#t #t #t)]

; the first tofu is picked up on the second conde as first tofu of '(tofu d tofu)
; the second as the first tofu in last recirsion with '(tofu)
; and the third as the first conde and last tofu of '(tofu)
; therefore three #t
; DOUBT !!!! Why 3 #t, shouldn't porrompompero be independent of (== #t q)?
; ------------------------------------------------------------------------------
; to understand why let's get back to basics. Remember from ch21:

[check-equal?
 (run* (x)
       (conde
        [(== 'virgin x) u#] ; throw away
        [(== 'olive  x) u#] ; throw away
        [s# s#]             ; x refreshed, s# succeeds => answer success => store (but without association)
        [u# s#]             ; fails, throw away
        [(== 'oil x)    u#] ; throw away
        [u#]))
 '(_.0)]

[check-equal?
 (run* (q)
       (conde    ; conde_1
        (s#)     ; scope_of_goals_a
        (s#)     ; scope_of_goals_b
        (u#)     ; scope_of_goals_bad
        (s#))    ; scope_of_goals_c
       (conde    ; conde_2
        (s#)     ; scope_of_goals_d
        (s#)))   ; scope_of_goals_e
 '(_.0 _.0 _.0 _.0 _.0 _.0)]

; For run* we look at:
; at the top-level inside the run* we have goals separated by ANDs: conde_1 && conde_2
; inside a conde there are ORs => scope_of_goals_i || scope_of_goals_j
; inside a scope_of_goals_i we have ANDS again (g1 && g2 && ...), but we don't care now for that depth of detail
; So the combinations (segment 82-84) make sense because
; (a || b || c) && ( d || e)  => [a && d] || [a && e] || [b && d] || [b && e] || [c && e] || [c && e].
; It makes sense from a purely logical sense: a set of subsets of goals.
;
; A combination of scopes_of_goals is what intuitively sounds like:
; [a && d] ==> like a union of scopes => a list of all the included goals => scope_of_goals_a U scope_of_goals_d => goals_of_a && goals_f_d
;
; That explanation makes coherent the  following:

[check-equal?
 (run* (q)
       (conde
        (s#)      ; scope_1 (a list of goals)
        (s#)      ; scope_2 (a list of goals)
        (u#))     ; forget about this one
       (== 1 q))  ; unification_goal
 '(1 1)]

; where the solutions of q is '(1 1), because we have 2 scopes,
; like 2 scenarios (with each having its own subset of goals):
;   scope_1 U unification_goal => (s# && unification_goal)
;   scope_2 U unification_goal => (s# && unification_goal)
;
; And each scenario, scope, subset finds an independent solution for q (although in this case both coincide).
; ------------------------------------------------------------------------------

; # 86
(define pinturero ; more pmembero
  (lambda (x l)
    (conde
     ((eq-caro l x) (cdro l '()))                ; [1] picks the last
     ((eq-caro l x) (fresh (a d)
                           (cdro l `(,a . ,d)))) ; [2] make sure is not last because there is a cdro
     ((fresh (d)
             (cdro l d)
             (pinturero x d))))))

; # 88
[check-equal?
 (run* (q)
       (pinturero 'tofu '(a b tofu d tofu))
       (== #t q))
 '(#t #t)]

; # 89 to 92
[check-equal?
 (run 12 (l)
      (pinturero 'tofu l))
 '((tofu)                                 ; proper
  (tofu _.0 . _.1)                        ;        bad tofu
  (_.0 tofu)                              ; proper
  (_.0 tofu _.1 . _.2)                    ;        bad tofu
  (_.0 _.1 tofu)                          ; proper
  (_.0 _.1 tofu _.2 . _.3)                ;        bad tofu
  (_.0 _.1 _.2 tofu)                      ; proper
  (_.0 _.1 _.2 tofu _.3 . _.4)            ;        bad tofu
  (_.0 _.1 _.2 _.3 tofu)                  ; proper
  (_.0 _.1 _.2 _.3 tofu _.4 . _.5)        ;        bad tofu
  (_.0 _.1 _.2 _.3 _.4 tofu)              ; proper
  (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6))]  ;        bad tofu

; why the alternation? because our conde has 2 exit condes, when is last and when is not
; when it hits [1] and solves for x => proper, and when the sol is not last => bad tofu
; each consecutive 2 solutions correspond to the 2 condes where it finds the sol [1] and [2]

; # 93 & 94
(define contra-pinturero ; reversed order pinturero
  (lambda (x l)
    (conde
     ((eq-caro l x) (fresh (a d)
                           (cdro l `(,a . ,d)))) ; [2] make sure is not last because there is a cdro
     ((eq-caro l x) (cdro l '()))                ; [1] picks the last
     ((fresh (d)
             (cdro l d)
             (contra-pinturero x d))))))

[check-equal?
 (run 6 (l)
      (contra-pinturero 'tofu l))
 '((tofu _.0 . _.1)                       ;        bad tofu
  (tofu)                                  ; proper
  (_.0 tofu _.1 . _.2)                    ;        bad tofu
  (_.0 tofu)                              ; proper
  (_.0 _.1 tofu _.2 . _.3)                ;        bad tofu
  (_.0 _.1 tofu))]                        ; proper

; we just change the order of the condes that are going to determine the solution

; # 95
(define first-value
  (lambda (l)
    (run 1 (y)
         (membrillo y l))))

; gives us the first solution of (membrillo _.0 l), which we know is '() or the car

; # 96 & 97
[check-equal? (first-value '(pasta e fagioli)) '(pasta)]

; # 98 & 99
(define memberroblleaahh ; onomatopoeia for barf after way too much f###ing fagioli
  (lambda (x l)
    (conde
     ((fresh (d)
             (cdro l d)
             (memberroblleaahh x d)))
     ((eq-caro l x)))))

; (define membrillo ; for comparison purposes
;   (lambda (x l)
;     (conde
;      ((eq-caro l x))
;      ((fresh (b)
;              (cdro l b)
;              (membrillo x b))))))

; the same as membrillo but we changed the order of the conditions, so probably the order of
; solutions will change too?

; # 100
[check-equal?
 (run* (x)
       (memberroblleaahh x '(pasta e fagioli)))
 '(fagioli e pasta)]

; cute, it is ollirbmem (membrillo reversed)

; # 101
(define reverse-list
  (lambda (l)
    (run* (y) (memberroblleaahh y l))))
