#lang racket/base

(require rackunit)
(require minikanren)
(require "reasoned.rkt")

;; 1
(define c 42)

(check-equal?
  (let ([x (lambda (a) a)]
        [y c])
    (x y))
  c)

;; 2
(check-run* (r)
            (fresh (y x)
                   (== (list x y) r))
            => '((_.0 _.1)))

;; 3
(check-run* (r)
            (fresh (v w)
                   (== (let ([x v]
                             [y w])
                         (list x y)) r))
            => '((_.0 _.1)))

;; 4
(check-equal? (car '(grape rasin pear)) 'grape)

;; 5
(check-equal? (car '(a c o r n)) 'a)

;; 9
(define (caro p a)
  (fresh (d)
         (== (cons a d) p)))

;; 6
(check-run* (r)
            (caro '(a c o r n) r)
            => '(a))

;; 7
(check-run* (q)
            (caro '(a c o r n) 'a)
            (== #t q)
            => '(#t))

;; 8
(check-run* (r)
            (fresh (x y)
                   (caro (list r y) x)
                   (== 'pear x))
            => '(pear))

;; 10
(check-equal? (cons
                (car '(grape raisin pear))
                (car '((a) (b) (c))))
              '(grape a))

;; 11
(check-run* (r)
            (fresh (x y)
                   (caro '(grape raisin pear) x)
                   (caro '((a) (b) (c)) y)
                   (== (cons x y) r))
            => '((grape a)))

;; 13
(check-equal? (cdr '(grape raisin pear)) '(raisin pear))

;; 14
(check-equal? (car (cdr '(a c o r n))) 'c)

;; 16
(define (cdro p d)
  (fresh (a)
         (== (cons a d) p)))

;; 15
(check-run* (r)
            (fresh (v)
                   (cdro '(a c o r n) v)
                   (caro v r))
            => '(c))

;; 17
(check-equal? (cons
                (cdr '(grape raisin pear))
                (car '((a) (b) (c))))
              '((raisin pear) a))

;; 18
(check-run* (r)
            (fresh (x y)
                   (cdro '(grape raisin pear) x)
                   (caro '((a) (b) (c)) y)
                   (== (cons x y) r))
            => '(((raisin pear) a)))

;; 19
(check-run* (q)
            (cdro '(a c o r n) '(c o r n))
            (== #t q)
            => '(#t))

;; 20
(check-run* (x)
            (cdro '(c o r n) `(,x r n))
            => '(o))

;; 21
(check-run* (l)
            (fresh (x)
                   (cdro l '(c o r n))
                   (caro l x)
                   (== 'a x))
            => '((a c o r n)))

;; 28
(define (conso a d p)
  (== (cons a d) p))

;; 22
(check-run* (l)
            (conso '(a b c) '(d e) l)
            => '(((a b c) d e)))

;; 23
(check-run* (x)
            (conso x '(a b c) '(d a b c))
            => '(d))

;; 24
(check-run* (r)
            (fresh (x y z)
                   (== `(e a d ,x) r)
                   (conso y `(a ,z c) r))
            => '((e a d c)))

;; 25
(check-run* (x)
            (conso x `(a ,x c) `(d a ,x c))
            => '(d))

;; 26
(check-run* (l)
            (fresh (x)
                   (== `(d a ,x c) l)
                   (conso x `(a ,x c) l))
            => '((d a d c)))

;; 27
(check-run* (l)
             (fresh (x)
                    (conso x `(a ,x c) l)
                    (== `(d a ,x c) l))
             => '((d a d c)))

;; 29
(check-run* (l)
            (fresh (d x y w s)
                   (conso w '(a n s) s)
                   (cdro l s)
                   (caro l x)
                   (== 'b x)
                   (cdro l d)
                   (caro d y)
                   (== 'e y))
            => '((b e a n s)))

;; 30
(check-equal? (null? '(grape raisin pear)) #f)

;; 31
(check-equal? (null? '()) #t)

;; 35
(define (nullo x)
  (== x '()))

;; 32
(check-run* (q)
            (nullo '(grape raisin pear))
            (== #t q)
            => '())

;; 33
(check-run* (q)
            (nullo '())
            (== #t q)
            => '(#t))

;; 34
(check-run* (x)
            (nullo x)
            => '(()))

;; 36
(check-equal? (eq? 'pear 'plum) #f)

;; 37
(check-equal? (eq? 'plum 'plum) #t)

;; 40
(define (eqo x y)
  (== x y))

;; 38
(check-run* (q)
            (eqo 'pear 'plum)
            (== #t q)
            => '())

;; 39
(check-run* (q)
            (eqo 'plum 'plum)
            (== #t q)
            => '(#t))

;; 43
(check-true (pair? '(split . pea)))

;; 44
(check-true (pair? '((split) . pea)))

;; 45
(check-false (pair? 'pair))

;; 46
(check-false (pair? 'pear))

;; 47
(check-true (pair? '(pear)))

;; 48
(check-equal? (car '(pear)) 'pear)

;; 49
(check-equal? (cdr '(pear)) '())

;; 51
(check-equal? (cons '(split) 'pea) '((split) . pea))

;; 52
(check-run* (r)
            (fresh (x y)
                   (== (cons x (cons y 'salad)) r))
            => '((_.0 _.1 . salad)))

;; 53
(define (pairo p)
  (fresh (a d)
    (conso a d p)))

;; not recursive! o_O

;; 54
(check-run* (q)
            (pairo (cons q q))
            (== #t q)
            => '(#t))

;; 55
(check-run* (q)
            (pairo '())
            (== #t q)
            => '())

;; 56
(check-run* (q)
            (pairo 'pair)
            (== #t q)
            => '())

;; 57
(check-run* (x)
            (pairo x)
            => '((_.0 . _.1)))

;; 58
(check-run* (r)
            (pairo (cons r 'pear))
            => '(_.0))

;; 60
;; Oui.
;; Méfiez-vous des ombres. (Beware of shadows)

(define (caro* p a)
  (fresh (d)
         (conso a d p)))

(check-run* (r)
            (caro* '(a c o r n) r)
            => '(a))

(define (cdro* p d)
  (fresh (a)
         (conso a d p)))

(check-run* (r)
            (fresh (v)
                   (cdro* '(a c o r n) v)
                   (caro* v r))
            => '(c))

;; pario already defined in terms of conso
