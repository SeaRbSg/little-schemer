#lang racket/base

(require "lib/reasonable.rkt")

(define (cons° a d l)
  (≈ (cons a d) l))

(define (car° p a)
  (fresh (d)
    (cons° a d p)))

(define (cdr° p d)
  (fresh (a)
    (cons° a d p)))

(define (pair° p)
  (fresh (a d)
    (cons° a d p)))

(define (null° l)
  (≈ '() l))

(define (eq° a b)
  (≈ a b))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (r)                       ; 2
              (fresh (y x)
                (≈ (list x y) r))
              => '((_.0 _.1)))

  (check-run* (r)                       ; 3
              (fresh (v w)
                (≈ (let ([x v]
                         [y w])
                     (list x y))
                   r))
              => '((_.0 _.1)))

  (check-run* (r)                       ; 5 -- this seems wrong, should be 'a
              (car° '(a c o r n) r)
              => '(a))

  (check-run* (q)                       ; 7
              (car° '(a c o r n) 'a)
              (≈ #t q)
              => '(#t))

  (check-run* (r)                       ; 8
              (fresh (x y)
                (car° (list r y) x)
                (≈ 'pear x))
              => '(pear))

  (check-run* (r)                       ; 11
              (fresh (x y)
                (car° '(grape raisin pear) x)
                (car° '((a) (b) (c)) y)
                (≈ (cons x y) r))
              => '((grape a)))

  (check-run* (r)                       ; 15
              (fresh (v)
                (cdr° '(a c o r n) v)
                (car° v r))
              => '(c))

  (check-run* (r)                       ; 18
              (fresh (x y)
                (cdr° '(grape raisin pear) x)
                (car° '((a) (b) (c)) y)
                (≈ (cons x y) r))
              => '(((raisin pear) a)))

  (check-run* (q)                       ; 19
              (cdr° '(a c o r n) '(c o r n))
              (≈ #t q)
              => '(#t))

  (check-run* (x)                       ; 20
              (cdr° '(c o r n) `(,x r n))
              => '(o))

  (check-run* (l)                       ; 21
              (fresh (x)
                (cdr° l '(c o r n))
                (car° l x)
                (≈ 'a x))
              => '((a c o r n)))

  (check-run* (l)                       ; 22
              (cons° '(a b c) '(d e) l)
              => '(((a b c) d e)))

  (check-run* (x)                       ; 23
              (cons° x '(a b c) '(d a b c))
              => '(d))

  (check-run* (r)                       ; 24
              (fresh (x y z)
                (≈ `(e a d ,x) r)
                (cons° y `(a ,z c) r))
              => '((e a d c)))

  (check-run* (x)                       ; 25
              (cons° x `(a ,x c) `(d a ,x c))
              => '(d))

  (check-run* (l)                       ; 26
              (fresh (x)
                (≈ `(d a ,x c) l)
                (cons° x `(a ,x c) l))
              => '((d a d c)))

  (check-run* (l)                       ; 27
              (fresh (x)
                (cons° x `(a ,x c) l)
                (≈ `(d a ,x c) l))
              => '((d a d c)))

  (check-run* (l)                       ; 29
              (fresh (d x y w s)
                (cons° w '(a n s) s)    ; s => `(,w a n s)
                (cdr° l s)              ; l => `(_.0 ,w a n s)
                (car° l x)              ; l => `(,x ,w a n s)
                (≈ 'b x)                ; l => `(b ,w a n s)
                (cdr° l d)              ; d => `(,w a n s) => s
                (car° d y)              ; w => y
                (≈ 'e y))               ; l => `(b e a n s)
              => '((b e a n s)))

  (check-run* (q)                       ; 32
              (null° '(grape raisin pear))
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 33
              (null° '())
              (≈ #t q)
              => '(#t))

  (check-run* (x)                       ; 34
              (null° x)
              => '(()))

  (check-run* (q)                       ; 38
              (eq° 'pear 'plum)
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 39
              (eq° 'plum 'plum)
              (≈ #t q)
              => '(#t))

  (check-run* (r)                       ; 52
              (fresh (x y)
                (≈ (list x y 'salad) r))
              => '((_.0 _.1 salad)))

  (check-run* (q)                       ; 54
              (pair° (cons q q))
              (≈ #t q)
              => '(#t))

  (check-run* (q)                       ; 55
              (pair° '())
              (≈ #t q)
              => none)

  (check-run* (q)                       ; 56
              (pair° 'pair)
              (≈ #t q)
              => none)

  (check-run* (x)                       ; 57
              (pair° x)
              => '((_.0 . _.1)))

  (check-run* (r)                       ; 58
              (pair° (cons r 'pear))
              => '(_.0))
  )
