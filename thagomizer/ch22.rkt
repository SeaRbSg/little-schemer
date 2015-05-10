#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

[check-equal? (let ((x (lambda (a) a))
                    (y 'c))
                (x y))
              'c]


[check-equal? (run* (r)
                    (fresh (y x)
                           (== `(,x ,y) r)))
              '((_.0 _.1))]


[check-equal? (run* (r)
                    (fresh (v w)
                           (== (let ((x v) (y w)) `(,x ,y)) r)))
              '((_.0 _.1))]


[check-equal? (car `(grape raisin pear)) 'grape]


[check-equal? (car `(a c o r n)) 'a]


[check-equal? (run* (r)
                    (caro `(a c o r n) r))
              '(a)]


[check-equal? (run* (q)
                    (caro `(a c o r n) 'a)
                    (== #t q))
              '(#t)]


[check-equal? (run* (r)
                    (fresh (x y)
                           (caro `(,r ,y) x)
                           (== 'pear x)))
              '(pear)]


[check-equal? (cons 
               (car `(grape raisin pear))
               (car `((a) (b) (c))))
              
              '(grape a)]


[check-equal? (run* (r)
                    (fresh (x y)
                           (caro `(grape raisin pear) x)
                           (caro `((a) (b) (c)) y)
                           (== (cons x y) r)))
              '((grape a))]


[check-equal? (cdr `(grape raisin pear)) `(raisin pear)]


[check-equal? (car (cdr `(a c o r n))) 'c]


[check-equal? (run* (r)
                    (fresh (v)
                           (cdro `(a c o r n) v)
                           (caro v r)))
              '(c)]


[check-equal? (cons (cdr `(grape raisin pear))
                    (car `((a) (b) (c))))
              '((raisin pear) a)]

[check-equal? (run* (r)
                    (fresh (x y)
                           (cdro `(grape raisin pear) x)
                           (caro `((a) (b) (c)) y)
                           (== (cons x y) r)))
              '(((raisin pear) a))]


[check-equal? (run* (q)
                    (cdro `(a c o r n) `(c o r n))
                    (== #t q))
              '(#t)]


[check-equal? (run* (x)
                    (cdro `(c o r n) `(,x r n)))
              '(o)]


[check-equal? (run* (l)
                    (fresh (x)
                           (cdro l '(c o r n))
                           (caro l x)
                           (== 'a x)))
              '((a c o r n))]


[check-equal? (run* (l)
                    (conso '(a b c) '(d e) l))
              '(((a b c) d e))]


[check-equal? (run* (x)
                    (conso x '(a b c) '(d a b c)))
              '(d)]


[check-equal? (run* (r)
                    (fresh (x y z)
                           (== `(e a d ,x) r)      ;; (e a d _.0)
                           (conso y `(a ,z c) r))) ;; (e a d c)
              '((e a d c))]


[check-equal? (run* (x)
                    (conso x `(a ,x c) `(d a ,x c)))
              '(d)]


[check-equal? (run* (l)
                    (fresh (x) 
                           (== `(d a ,x c) l)
                           (conso x `(a ,x c) l)))
              '((d a d c))]


[check-equal? (run* (l)
                    (fresh (x)
                           (conso x `(a ,x c) l)
                           (== `(d a ,x c) l)))
              '((d a d c))]


[check-equal? (run* (l)
                    (fresh (d x y w s)
                           (conso w '(a n s) s)   
                           (cdro l s)
                           (caro l x)
                           (== 'b x)
                           (cdro l d)
                           (caro d y)
                           (== 'e y)))
              '((b e a n s))]


[check-equal? (null? '(grape raisin pear)) #f]


[check-equal? (null? '()) #t]


[check-equal? (run* (q)
                    (nullo '(grape raisin pear))
                    (== #t q))
              '()]


[check-equal? (run* (q)
                    (nullo '())
                    (== #t q))
              '(#t)]


[check-equal? (eq? 'pear 'plum) #f]


[check-equal? (eq? 'plum 'plum) #t]


[check-equal? (run* (q)
                    (eqo 'pear 'plum)
                    (== #t q))
              '()]


[check-equal? (run* (q)
                    (eqo 'plum 'plum)
                    (== #t q))
              '(#t)]


[check-true (pair? '(split . pea))]
[check-true (pair? '((split) . pea))]

[check-false (pair? '())]
[check-false (pair? 'pair)]
[check-false (pair? 'pear)]

[check-true (pair? '(pear))]

[check-equal? (car '(pear)) 'pear]
[check-equal? (cdr '(pear)) '()]


[check-equal? (cons '(split) 'pea) '((split) . pea)]

[check-equal? (run* (r)
                    (fresh (x y)
                           (== (cons x (cons y 'salad)) r)))
              '((_.0 _.1 . salad))]


[check-equal? (run* (q)
                    (pairo (cons q q))
                    (== #t q))
              '(#t)]


[check-equal? (run* (q)
                    (pairo '())
                    (== #t q))
              '()]


[check-equal? (run* (q)
                    (pairo 'pair)
                    (== #t q))
              '()]


[check-equal? (run* (x)
                    (pairo x))
              '((_.0 . _.1))]


[check-equal? (run* (r)
                    (pairo (cons r 'pear)))
              '(_.0)]
