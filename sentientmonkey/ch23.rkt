#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch22.rkt")

;; 1
(define (list? l)
  (cond
    [(null? l) #t]
    [(pair? l) (list? (cdr l))]
    [else #f]))

(check-true (list? '((a) (a b) (c))))

;; 2
(check-true (list? '()))

;; 3
(check-false (list? 's))

;; 4
(check-false (list? '(d a t e . s)))

;; interesting. dotted pairs are not "proper" lists.

;; 5
(define (listo l)
  (conde
    [(nullo l) s#]
    [(pairo l)
     (fresh (d)
       (cdro l d)
       (listo d))]
    [else u#]))

;; we are recusing through a list to return #s or #u vs. #t or #f

;; 6
;; fresh introduces d, which allows us to state the two goals:
;;  1. the cdr of l unifies with d
;;  2. d should also unify with a list

;; 7
(check-run* (x)
            (listo `(a b ,x d))
            => '(_.0))

;; 8
;; the goal is determing the value of x, not the return of listo

;; 9
;; at the end of the listo recursion, x is still _.0

;; 10
(check-run 1 (x)
           (listo `(a b c . ,x))
           => '(()))

;; 11
;; listo is only successfull when x is a null list.

;; 12
;; because...
(check-run* (x)
            (nullo x)
            => '(()))

;; 13
;;(check-run* (x)
;;           (listo `(a b c . ,x))
;;           => '(()))

;; never finishes...

;; 14
(check-run 5 (x)
           (listo `(a b c . ,x))
           => '(()
                (_.0)
                (_.0 _.1)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3)))
;; holy listo, batman!

;; 15
;; conde is not like cond at all. cond makes a decision, while conde contiunes to evaluate all branches.

;; 16
(define (lol? l)
  (cond
    [(null? l) #t]
    [(list? (car l)) (lol? (cdr l))]
    [else #f]))

;; lol? returns true if we have a list-of-lists.
;; aka turtle lists all the way down.

(check-true (lol? '((a (b (c))))))
(check-true (lol? '(())))
(check-false (lol? '(a)))
(check-true (lol? '())) ; but I disagree on this one ;)

;; 17
(define (lolo l)
  (conde
    [(nullo l) s#]
    [(fresh (a)
       (caro l a)
       (listo a))
     (fresh (d)
       (cdro l d)
       (lolo d))]
    [else u#]))

;; 20
(check-run 1 (l)
           (lolo l)
           => '(()))

;; 21
(check-run* (q)
            (fresh (x y)
                   (lolo `((a b) (,x c) (d ,y)))
                   (== #t q))
            => '(#t))

;; 22
(check-run 1 (q)
            (fresh (x y)
                   (lolo `((a b) . ,x))
                   (== #t q))
            => '(#t))

;; 23
(check-run 1 (x)
           (lolo `((a b) (c d) . ,x))
           => '(()))

;; 24
(check-run 5 (x)
           (lolo `((a b) (c d) . ,x))
           => '(()
                (())
                (() ())
                (() () ())
                (() () () ())))
;; trololololo...

;; 25
(check-equal?
  '((a b) (c d) . (() () ()))
  '((a b) (c d) () () ()))

;; huh. dotted pairs keep getting stranger and stranger.

;; 31
(define (twinso s)
  (fresh (x y)
    (conso x y s)
    (conso x '() y)))

;; 32
(check-run* (q)
            (twinso '(tofu tofu))
            (== #t q)
            => '(#t))

;; 33
(check-run* (z)
            (twinso `(,z tofu))
            => '(tofu))

;; 36
(define (twinso* s)
  (fresh (x)
    (== `(,x ,x) s)))

(check-run* (z)
            (twinso* `(,z tofu))
            => '(tofu))

;; 37
;; listo-of-twinso
(define (loto l)
  (conde
    [(nullo l) s#]
    [(fresh (a)
       (caro l a)
       (twinso a))
     (fresh (d)
       (cdro l d)
       (loto d))]
    [else u#]))

;; 38
(check-run 1 (z)
           (loto `((g g) . ,z))
           => '(()))

;; 39
;; because...
(check-run 1 (q)
           (twinso '(g g))
           (== #t q)
           => '(#t))

(check-run 1 (z)
           (nullo z)
           => '(()))

;; 40
(check-equal?
  '((g g) . ())
  '((g g)))

;; 42
(check-run 5 (z)
           (loto `((g g) . ,z))
           => '(()
                ((_.0 _.0))
                ((_.0 _.0) (_.1 _.1))
                ((_.0 _.0) (_.1 _.1) (_.2 _.2))
                ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))))
;; that's a loto.

;; 43
;; each of these are new options for twins

;; 44
(check-equal?
  '((g g) . ((_.0 _.0) (_.1 _.1) (_.2 _.2)))
  '((g g) (_.0 _.0) (_.1 _.1) (_.2 _.2)))

;; 45
(check-run 5 (r)
           (fresh (w x y z)
             (loto `((g g) (e ,w) (,x ,y) . ,z))
             (== `(,w (,x ,y) ,z) r))
           => '((e (_.0 _.0) ())
                (e (_.0 _.0) ((_.1 _.1)))
                (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
                (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
                (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))))
;; eeep. o_O

;; 46
(check-equal?
  '((g g) (e e) (_.0 _.0) . ((_.1 _.1) (_.2 _.2)))
  '((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))

;; 47
(check-run 3 (out)
           (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (loto out))
           => '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

;; 48
(define (listofo predo l)
  (conde
    [(nullo l) s#]
    [(fresh (a)
       (caro l a)
       (predo a))
     (fresh (d)
       (cdro l d)
       (listofo predo d))]
    [else u#]))

;; 49
(check-run 3 (out)
           (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (listofo twinso out))
           => '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))
;; old friends, new tricks indeed.

;; 50
(define (loto* l)
  (listofo twinso l))

(check-run 3 (out)
           (fresh (w x y z)
             (== `((g g) (e ,w) (,x ,y) . ,z) out)
             (loto* out))
           => '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

;; 51
;; how could I forget?
(define (member? x l)
  (cond
    [(null? l) #f]
    [(eq-car? l x) #t]
    [else (member? x (cdr l))]))

(define (eq-car? l x)
  (eq? (car l) x))

;; 53
(check-true (member? 'olive '(virgin olive oil)))

;; 54
(define (eq-caro l x)
  (caro l x))

(define (membero x l)
  (conde
    [(nullo l) u#]
    [(eq-caro l x) s#]
    [else
      (fresh (d)
        (cdro l d)
        (membero x d))]))
;; 55
;; I guess? I think I see why.

;; 56
;; (member? x (cdr l))

;; 57
(check-run* (q)
            (membero 'olive '(virgin olive oil))
            (== #t q)
            => '(#t))

;; 58
(check-run 1 (y)
           (membero y '(hummus with pita))
           => '(hummus))

;; that is more interesting.

;; 59
(check-run 1 (y)
           (membero y '(with pita))
           => '(with))

;; 60
(check-run 1 (y)
           (membero y '(pita))
           => '(pita))

;; 61
(check-run* (y)
            (membero y '())
            => '())

;; 62
(check-run* (y)
            (membero y '(hummus with pita))
            => '(hummus with pita))

;; 63
;; nullo is unsuccessful each time through, so y gets refreshed

;; 64
;; seems like it.

;; 65
(define (identity l)
  (run* (y)
    (membero y l)))

(check-equal? (identity '(hummus with pita)) '(hummus with pita))

;; 66
(check-run* (x)
            (membero 'e `(paste ,x fagioli))
            => '(e))
;; 69
(check-run 1 (x)
           (membero 'e `(paste e ,x fagioli))
           => '(_.0))

;; 70
(check-run 1 (x)
           (membero 'e `(pasta ,x e fagioli))
           => '(e))

;; 71
(check-run* (r)
            (fresh (x y)
              (membero 'e `(pasta ,x fagioli ,y))
              (== `(,x ,y) r))
            => '((e _.0) (_.0 e)))
;; ooh. weird.

;; 72
;; we have two sets of values for x and y because e could be either x or y.

;; 73
(check-run 1 (l)
           (membero `tofu l)
           => '((tofu . _.0)))
