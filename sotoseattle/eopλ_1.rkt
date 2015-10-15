#lang eopl
(require racket)
(require rackunit)
(require racket/trace)

; 1.1 derive syntactically: (-7 . (3 .(14 . ())))

;    <list-of-numbers>
; => (number . <list-of-numbers>)
; => (-7 . <list-of-numbers>)
; => (-7 . (number . <list-of-numbers>))
; => (-7 . (3 . <list-of-numbers>))
; => (-7 . (3 . (number . <list-of-numbers>)))
; => (-7 . (3 . (14 . <list-of-numbers>)))
; => (-7 . (3 . (14 . ())))

; 1.2 rewrite the <datum> grammar and prove that (#t (foo . ()) 3) is a list.

; <list>         ::= () | (<datum>)
; <dotted-datum> ::= (<datum> . <datum>)
; <datum>        ::= <number> | <symbol> | <boolean> | <string>
;                ::= <list> | <dotted-datum> | <vector>

;    <list>
; => (<datum>)
; => (<dotted-datum>)
; => (<datum> . <datum>)
; => (<datum> . <dotted-datum>)
; => (<datum> (<datum> . <datum>))
; => (<boolean> (<datum> . <datum>))
; => (#t (<datum> . <datum>))
; => (#t (<dotted-datum> . <datum>))
; => (#t (<datum> . <datum>) . <datum>)
; => (#t (<datum> . <datum>) <datum>)
; => (#t (<symbol> . <datum>) <datum>)
; => (#t (foo . <datum>) <datum>)
; => (#t (foo . <list>) <datum>)
; => (#t (foo . ()) <datum>)
; => (#t (foo . ()) <number>)
; => (#t (foo . ()) 3)

; 1.3 Prove that (a "mixed" # (bag (of . data))) is a datum

;    <datum>
; => <list>
; => (<datum> <datum> <datum> <datum>)
; => (<datum> <datum> <datum> <list>)
; => (<datum> <datum> <datum> (<datum> <datum>))
; => (<datum> <datum> <datum> (<datum> <list>))
; => (<datum> <datum> <datum> (<datum> (<datum>)))
; => (<datum> <datum> <datum> (<datum> (<dotted-datum>)))
; => (<datum> <datum> <datum> (<datum> (<datum> . <datum>)))
; => (<symbol> <datum> <datum> (<datum> (<datum> . <datum>)))
; => (a <datum> <datum> (<datum> (<datum> . <datum>)))
; => (a <string> <datum> (<datum> (<datum> . <datum>)))
; => (a "mixed" <datum> (<datum> (<datum> . <datum>)))
; => (a "mixed" <symbol> (<datum> (<datum> . <datum>)))
; => (a "mixed" # (<datum> (<datum> . <datum>)))
; => (a "mixed" # (<symbol> (<datum> . <datum>)))
; => (a "mixed" # (bag (<datum> . <datum>)))
; => (a "mixed" # (bag (<symbol> . <datum>)))
; => (a "mixed" # (bag (of . <datum>)))
; => (a "mixed" # (bag (of . <symbol>)))
; => (a "mixed" # (bag (of . data)))

; What is wrong with (a . b . c)?

;    <datum>
; => <dotted-datum>
; => (<datum> . <datum>)
; => (<dotted-datum> . <datum>)
; => ((<datum> . <datum>) . <datum>)
; => ((<symbol> . <datum>) . <datum>)
; => ((a . <datum>) . <datum>)
; => ((a . <symbol>) . <datum>)
; => ((a . b) . <datum>)
; => ((a . b) . <symbol>)
; => ((a . b) . c)
; => ((a . b) c) =/= (a . b . c)  AM I RIGHT?

; 1.4 Prove that if e is an <expression> as defined in 1.1.2, then there are the same number of left and right parens in e.

; <expression> ::= <identifier>
;              ::= (lambda (<identifier>) <expression>)
;              ::= (<expression> <expression>)

; - if <expression> ::= <identifier> then there are no parens left or right. => OK
; - given that ex2 is an expression that holds the theorem true (has matching parens)
;   <expression> ::= (lambda (<identifier>) <ex2>) will have 2 sets of matching parens (we assume ex2 holds) => OK
; - given that ex3 and ex4 hold true (each and both have matching parens)
;   <expression> ::= (<ex3> <ex4>) will have a single set of matching parents (we assume ex3 and ex4 hold) => OK
; - since
;     a) at each level of recursion we have matching parens, AND
;     b) ex2, ex3 and ex4 can only be the 3 previously defined entities
;   we have 2 options
;     - all <expressions> end in an <identifier> => and since parens match at each level of recursion => OK
;     - one or more expressions never end => but we do know that as long as we evaluate parens match at each level of recursion, so the theorem holds at each level ad infinitum. It never does not hold => there is no case where it does not hold => holds => OK

;     HA HA HA!

; Much better, by induction if it holds for k it holds for k+1 => translate to levels of recursion instead of natural numbers
; It holds for ex2 => holds for one level up => holds for (lambda (<identifier>) <ex2>)
; It holds for ex3, ex4 => holds for one level up => holds for (<ex3> <ex4>)

(define list-of-numbers?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(number? (car lat)) (list-of-numbers? (cdr lat))]
      [else #f])))

(test-true  "nice" (list-of-numbers? '(1 2 3)))
(test-false "bad"  (list-of-numbers? '(1 (2) 3)))
(test-false "bad"  (list-of-numbers? '(1 'w 3)))

; 1.5 Extend the above from lists to datums

(define list-of-numbers-dat?
  (lambda (datum)
    (cond
      [(not (list? datum)) #f]  ; <== the only line I add
      [(null? datum) #t]
      [(number? (car datum)) (list-of-numbers-dat? (cdr datum))]
      [else #f])))

(test-false "string aint no list" (list-of-numbers-dat? 'casa))
(test-false "boolean aint a list" (list-of-numbers-dat? #t))
(test-false "number aint no list" (list-of-numbers-dat? 3))
(test-true  "empty" (list-of-numbers-dat? '()))
(test-true  "nice" (list-of-numbers-dat? '(1 2 3)))
(test-false "list != number" (list-of-numbers-dat? '(1 (2) 3)))
(test-false "str  != number" (list-of-numbers-dat? '('one 'two)))
(test-false "str  != number" (list-of-numbers-dat? '(1 'two 3)))

; return nth element of a numbered list

(define nth-ele
  (lambda (lat n)
    (cond
      [(null? lat) ; '()]
       (eopl:error 'nth-ele "List too short by ~s elements.~%" (+ n 1))]
      [(zero? n) (car lat)]
      [(nth-ele (cdr lat) (- n 1))])))

(test-equal? "first"  'a (nth-ele '(a b c) 0))
(test-equal? "second" 'b (nth-ele '(a b c) 1))
(test-equal? "last"   'c (nth-ele '(a b c) 2))
(check-exn exn:fail? (lambda () (nth-ele '(a b c) 3)))

(define list-length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [(+ 1 (list-length (cdr lat)))])))

(test-equal? "nice" 3 (list-length '(a b c)))
(test-equal? "nice" 2 (list-length '((x) ())))
(test-equal? "nice" 0 (list-length '()))

; 1.6 Robustify the previous to procedures (check for arg type and raise error)

(define nth-ele-robust
  (lambda (lat n)
    (cond
      [(not (list? lat)) (eopl:error 'nth-ele-robust  "This shit aint no list!")]
      [(null? lat) (eopl:error 'nth-ele-robust "List too short by ~s elements.~%" (+ n 1))]
      [(zero? n) (car lat)]
      [(nth-ele-robust (cdr lat) (- n 1))])))

(define list-length-robust
  (lambda (lat)
    (cond
      [(not (list? lat)) (eopl:error 'nth-ele "This shit aint no list!")]
      [(null? lat) 0]
      [(+ 1 (list-length-robust (cdr lat)))])))

; 1.7 Rewrite the error message so it is more informative

(define nth-ele-freaky
  (lambda (lat n)
    (cond
      [(not (list? lat)) (eopl:error 'nth-ele-freaky  "This shit aint no list!")]
      [(letrec
              ([dig-it (lambda (l m)
                         (cond
                           [(null? l) (eopl:error 'nth-ele-freaky "~s has no element ~s.~%" lat n)]
                           [(zero? m) (car l)]
                           [else (dig-it (cdr l) (- m 1))]))])
              (dig-it lat n))])))

(check-exn exn:fail? (lambda () (nth-ele-freaky '(1 2 3) 9)))
; (nth-ele-freaky '(1 2 3) 9)

(define remove-first
  (lambda (s los)
    (cond
      [(null? los) '()]
      [(eq? s (car los)) (cdr los)]
      [else (cons (car los) (remove-first s (cdr los)))])))

(test-equal? "nothing"    '(a b c) (remove-first 'd '(a b c)))
(test-equal? "nice"       '(b c)   (remove-first 'a '(a b c)))
(test-equal? "good doggy" '(a c b) (remove-first 'b '(a b c b)))

; 1.8 what happens with

(define remove-first-wacked
  (lambda (s los)
    (cond
      [(null? los) los]
      [(eq? s (car los)) (cdr los)]
      [(remove-first-wacked s (cdr los))])))

; gives the cdr of the list after the first occurrence of s
(test-equal? "whack!" '(5 3) (remove-first-wacked 3 '(1 2 3 5 3)))

(define remobe
  (lambda (s los)
    (cond
      [(null? los) los]
      [(eq? s (car los)) (remobe s (cdr los))]
      [(cons (car los) (remobe s (cdr los)))])))

(test-equal? "nice" '(1 2 4 2 1) (remobe 3 '(1 2 3 4 3 2 1)))
(test-equal? "brutal" '() (remobe 3 '(3 3 3 3 3 3 3)))

; 1.9 What happens when we take out the cons in this way?

(define remobe2
  (lambda (s los)
    (cond
      [(null? los) los]
      [(eq? s (car los)) (remobe2 s (cdr los))]
      [(remobe2 s (cdr los))])))

; is the same as

(define remobe3
  (lambda (s los)
    (cond
      [(null? los) los]
      [else (remobe3 s (cdr los))])))

; which traverses the whole list and when it reaches the end, returns '()

(test-equal? "weird" '() (remobe2 8 '(1 2 3 4 3 2 1)))
(test-equal? "weird" '() (remobe2 3 '(1 2 3 4 3 2 1)))
(test-equal? "weird" '() (remobe2 3 '(1 2 3)))

; <s-list>     ::= ()
;              ::= (<symbol-exp> . <s-list>)
; <symbol-exp> ::= <symbol> | <s-list>

(define subst
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(list? (car los))
         (cons (subst new old (car los)) (subst new old (cdr los)))]
      [(eq? old (car los))
         (cons new (subst new old (cdr los)))]
      [(cons (car los) (subst new old (cdr los)))])))

(test-equal? "nice" '(1 2 5 4 5 5 5) (subst 5 3 '(1 2 3 4 5 3 3)))
(test-equal? "omph" '(a (a c) (a () d)) (subst 'a 'b '(b (b c) (b () d))))

; that is how I woudl do it. Now lets do it mutually recursive
; follow the grammar !!!! nothing more <==== Very Important

; <s-list>     ::= () | (<symbol-exp> . <s-list>)

(define subst-lst
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(cons (subst-sex new old (car los))
             (subst-lst new old (cdr los)))])))

; <symbol-exp> ::= <symbol> | <s-list>

(define subst-sex
  (lambda (new old sex)
    (cond
      [(list? sex) (subst-lst new old sex)]
      [(eq? old sex) new]
      [else sex])))

(test-equal? "nice" '(1 2 5 4 5 5 5) (subst-lst 5 3 '(1 2 3 4 5 3 3)))
(test-equal? "omph" '(a (a c) (a () d)) (subst-lst 'a 'b '(b (b c) (b () d))))
    
; Freaking genius!
    
; 1.10 Why? I ask my self...
; because at that point we know that se is not a symbol, so it has to be a list
; and when called subst (or subst-lst) on a list we are guaranteed to halt
; because the recursion is always on smaller pieces (car and cdr of the list)

; 1.11 Lets refactor back! inlining!

(define subst-lst2
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(let ([a (car los)] [d (cdr los)])
         (cons
          (if (list? a) (subst-lst2 new old a) (if (eq? old a) new a))
          (subst-lst2 new old d)))])))

(test-equal? "nice" '(1 2 5 4 5 5 5) (subst-lst2 5 3 '(1 2 3 4 5 3 3)))
(test-equal? "omph" '(a (a c) (a () d)) (subst-lst2 'a 'b '(b (b c) (b () d))))

; 1.12 dont know what is map!! Forget it!

; Lets try it again following the grammar

; <top-level> ::= <s-list>
; <s-list>     ::= ()
;              ::= (<symbol-exp> . <s-list>)
; <symbol-exp> ::= <symbol> | <s-list>

(define notate-depth-top
  (lambda (lis)
    (notate-depth-lst lis 0)))

(define notate-depth-lst
  (lambda (los n)
    (cond
      [(null? los) los]
      [(cons
        (notate-depth-sex (car los) n)
        (notate-depth-lst (cdr los) n))])))

(define notate-depth-sex
  (lambda (sex n)
    (cond
      [(symbol? sex) (cons sex (cons n '()))]
      [(notate-depth-lst sex (+ n 1))])))

(test-equal? "it works! and it is easy!"
             '((a 0) ((b 1) () (c 1)) (((d 2))) (e 0))
             (notate-depth-top '(a (b () c) ((d)) e)))

; 1.13 Again, forget about map

;(define vector-sum
;  (lambda (v)
;    (cond
;      [(

; 1.15

(define duple
  (lambda (n x)
    (cond
      [(zero? n) '()]
      [(cons x (duple (- n 1) x))])))

(test-equal? "1.15.1" '(3 3) (duple 2 3))
(test-equal? "1.15.1" '((ho ho) (ho ho) (ho ho)) (duple 3 '(ho ho)))
(test-equal? "1.15.1" '() (duple 0 '(blah)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <list> ::= () | ({<pair>*})
; <pair> ::= (<symbol> <symbol>)

(define invert-pair
  (lambda (pair)
    (cons (cadr pair) (cons (car pair) '()))))

(define invert
  (lambda (lap)
    (cond
      [(null? lap) lap]
      [(cons (invert-pair (car lap)) (invert (cdr lap)))])))

(test-equal? "1.15.2" '((1 a) (2 a) (1 b) (2 b)) 
             (invert '((a 1) (a 2) (b 1) (b 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define filter-in
  (lambda (pred los)
    (cond
      [(null? los) los]
      [(pred (car los)) (cons (car los) (filter-in pred (cdr los)))]
      [(filter-in pred (cdr los))])))

(test-equal? "1.15.3" '(2 7) (filter-in number? '(a 2 (1 3) b 7)))
(test-equal? "1.15.3" '(a foo) (filter-in symbol? '(a (b c) 17 foo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define every?
  (lambda (pred los)
    (cond
      [(null? los) #t]
      [(pred (car los)) (every? pred (cdr los))]
      [else #f])))

(test-equal? "1.15.4" #f (every? number? '(a b c 3 e)))
(test-equal? "1.15.4" #t (every? number? '(1 2 3 5 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exists?
  (lambda (pred los)
    (cond
      [(null? los) #f]
      [(pred (car los)) #t]
      [else (exists? pred (cdr los))])))

(test-equal? "1.15.5" #t (exists? number? '(a b c 3 e)))
(test-equal? "1.15.5" #f (exists? number? '(a b c d e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vector-index
  (lambda (pred v)
    (letrec
        ([traverse (lambda (n)
                     (cond
                       [(zero? n) #f]
                       [(pred (vector-ref v n)) n]
                       [else (traverse (- n 1))]))])
         (traverse (- (vector-length v) 1)))))

(test-equal? "1.15.6" 
             2 
             (vector-index (lambda (x) (eqv? x 'c)) '#(a b c d)))

(test-equal? "1.15.6" 
             'b 
             (vector-ref '#(a b c) 
                         (vector-index (lambda (x) (eqv? x 'b)) '#(a b c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-set
  (lambda (lis n x)
    (cond
      [(null? lis) lis]
      [(eq? (list-ref lis n) (car lis)) (cons x (cdr lis))]
      [else (cons (car lis) (list-set (cdr lis) (- n 1) x))])))

(test-equal? "1.15.7" (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(test-equal? "1.15.7" (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define product
  (lambda (los1 los2)
    (letrec
        ([make-pair (lambda (a b) (cons a (cons b '())))]
         [glue (lambda (l1 l2)
                 (cond
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))]
         [zip  (lambda (a los)
                 (cond
                   [(null? los) '()]
                   [else (cons (make-pair a (car los)) (zip a (cdr los)))]))])
      (cond
        [(null? los1) '()]
        [(glue (zip (car los1) los2)
               (product (cdr los1) los2))]))))

(test-equal? "1.15.8" (product '(a b c) '(x y)) 
                               '((a x) (a y) (b x) (b y) (c x) (c y)))

(test-equal? "1.15.8" (product '(a b) '(x)) '((a x) (b x)))
(test-equal? "1.15.8" (product '(a b) '()) '())
(test-equal? "1.15.8" (product '() '(x y)) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define down
  (lambda (los)
    (letrec
     ([wrap (lambda (x) (cons x '()))])
     (cond
       [(null? los) los]
       [(cons (wrap (car los)) (down (cdr los)))]))))

(test-equal? "1.15.9" (down '(1 2 3)) '((1) (2) (3)))
(test-equal? "1.15.9" (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(test-equal? "1.15.9" (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vector-append-lst
  (lambda (v l)
    (letrec ([glue (lambda (l1 l2)
                     (if (null? l1) l2 (cons (car l1) (glue (cdr l1) l2))))])
      (list->vector (glue (vector->list v) l)))))

(test-equal? "1.15.10" (vector-append-lst '#(1 2 3) '(4 5)) '#(1 2 3 4 5))
(test-equal? "1.15.10" (vector-append-lst '#(1 2 3) '()) '#(1 2 3))
(test-equal? "1.15.10" (vector-append-lst '#() '(hola)) '#(hola))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define up
  (lambda (l)
    (letrec 
        ([glue (lambda (l1 l2)
                 (cond
                   [(not (list? l1)) (cons l1 l2)]
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))])
      (cond
        [(null? l) l]
        [(glue (car l) (up (cdr l)))]))))
    

(test-equal? "1.16.1" (up '((1 2) (3 4))) '(1 2 3 4))
(test-equal? "1.16.1" (up '((x (y)) z)) '(x (y) z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define swapper
  (lambda (x y los)
    (cond
      [(null? los) los]
      [(list? (car los)) (cons (swapper x y (car los)) (swapper x y (cdr los)))]
      [(eq? x (car los)) (cons y (swapper x y (cdr los)))]
      [(eq? y (car los)) (cons x (swapper x y (cdr los)))]
      [else (cons (car los) (swapper x y (cdr los)))])))

(test-equal? "1.16.2" (swapper 'a 'd '(a b c d)) '(d b c a))
(test-equal? "1.16.2" (swapper 'a 'd '(a d () c d)) '(d a () c a))
(test-equal? "1.16.2" (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))
(test-equal? "1.16.2" (swapper 'x 'y '((x) y (w (z (x))))) '((y) x (w (z (y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define count-occurrences
  (lambda (s lis)
    (cond
      [(null? lis) 0]
      [(list? (car lis)) (+ (count-occurrences s (car lis)) (count-occurrences s (cdr lis)))]
      [(eq? s (car lis)) (+ 1 (count-occurrences s (cdr lis)))]
      [(count-occurrences s (cdr lis))])))

(test-equal? "1.16.3" (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(test-equal? "1.16.3" (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(test-equal? "1.16.3" (count-occurrences 'w '((f x) y (((x z) x)))) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flat!
  (lambda (lis)
    (letrec 
        ([glue (lambda (l1 l2)
                 (cond
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))])
      (cond
        [(null? lis) lis]
        [(list? (car lis)) (glue (flat! (car lis)) (flat! (cdr lis)))]
        [(cons (car lis) (flat! (cdr lis)))]
      ))))

(test-equal? "1.16.4" (flat! '(a b c)) '(a b c))
(test-equal? "1.16.4" (flat! '((a) () (b ()) () (c))) '(a b c))
(test-equal? "1.16.4" (flat! '((a b) c (((d)) e))) '(a b c d e))
(test-equal? "1.16.4" (flat! '(a b (() (c)))) '(a b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(define merge
  (lambda (lon1 lon2)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(> (car lon) n) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))])
      (cond
        [(null? lon1) lon2]
        [(merge (cdr lon1) (insert (car lon1) lon2))]))))

(test-equal? "1.16.5" (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(test-equal? "1.16.5" (merge '(35 62 81 90 91) '(3 83 85 90))
                             '(3 35 62 81 83 85 90 90 91))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define path
  (lambda (n bst)
    (let/cc jump
      (letrec
          ([left_branch  (lambda (l) (car (cdr l)))]
           [right_branch (lambda (l) (car (cdr (cdr l))))]
           [explore (lambda (b map)
                     (cond
                       [(null? b) '()]
                       [(eq? n (car b)) (jump (reverse map))]
                       [(explore (left_branch b) (cons 'left map))
                        (explore (right_branch b)(cons 'right map))]))])
        (explore bst '())))))

;                        14
;                      /    \
;                     /      \
;                    /        \
;                   7          26
;                    \        /  \
;                     \      /    \
;                     12    20     31
;                          /
;                         17

(test-equal? "1.17.1" (path 17 '(14 (7 () (12 () ()))
                                    (26 (20 (17 () ())
                                            ())
                                        (31 () ()))))
                      '(right left left))

(test-equal? "1.17.1" (path 14 '(14 (7 () (12 () ()))
                                    (26 (20 (17 () ())
                                            ())
                                        (31 () ()))))
                      '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sorti
  (lambda (l)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(> (car lon) n) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))]
         [worker (lambda (input output)
                (cond
                  [(null? input) output]
                  [(worker (cdr input) (insert (car input) output))]))])
      (worker l '()))))

(test-equal? "1.17.2" (sorti '(8 2 5 2 3)) '(2 2 3 5 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sortλ
  (lambda (λ l)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(λ n (car lon)) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))]
         [worker (lambda (input output)
                (cond
                  [(null? input) output]
                  [(worker (cdr input) (insert (car input) output))]))])
      (worker l '()))))


(test-equal? "1.17.3" (sortλ < '(8 2 5 2 3)) '(2 2 3 5 8))
(test-equal? "1.17.3" (sortλ > '(8 2 5 2 3)) '(8 5 3 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define composeλ
  (lambda (λ1 λ2)
    (lambda (x)
      (λ1 (λ2 x)))))

(test-equal? "1.18.1" ((composeλ car cdr) '(a b c d)) 'b)
(test-equal? "1.18.1" ((composeλ (composeλ car cdr) cdr) '(a b c)) 'c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define car&cdr
  (lambda (s slist errvalue)
    (cond
      [(null? slist) '()]
      [(eq? s (car slist)) car]
      [else (composeλ (car&cdr s (cdr slist) errvalue) cdr)])))

(test-equal? "1.18.2" (car&cdr 'a '(a b c) 'fail)
                       car)
(test-equal? "1.18.2" ((car&cdr 'c '(a b c) 'fail) '(a b c))
                      ((composeλ (composeλ car cdr) cdr) '(a b c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define car&cdr2
  (lambda (s slist errvalue)
    (lambda (x)
      (cond
        [(null? slist) '()]
        [(eq? s (car slist)) (car x)]
        [else ((car&cdr2 s (cdr slist) errvalue) (cdr x))]))))

(test-equal? "1.18.3" ((car&cdr2 'a '(a b c) 'fail) '(a b c))
                      (car '(a b c)))
(test-equal? "1.18.3" ((car&cdr2 'c '(a b c) 'fail) '(a b c))
                      (car (cdr (cdr '(a b c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






