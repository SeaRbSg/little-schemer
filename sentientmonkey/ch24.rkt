#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch22.rkt")
(require "ch23.rkt")

(provide rembero)

;; 1
(define (mem x l)
  (cond
    [(null? l) #f]
    [(eq-car? l x) l]
    [else (mem x (cdr l))]))

;; 2
(check-false (mem 'tofu '(a b peas d peads e)))

;; 3
(check-run* (out)
            (== (mem 'tofu '(a b tofu d peas e)) out)
            => '((tofu d peas e)))

;; 4
(check-equal? (mem 'peas
                   (mem 'tofu '(a b tofu d peas e)))
              '(peas e))

;; 5
(check-equal? (mem 'tofu
                   (mem 'tofu '(a b tofu d tofu e)))
              '(tofu d tofu e))

;; 6
(check-equal? (mem 'tofu
                   (cdr (mem 'tofu '(a b tofu d tofu e))))
              '(tofu e))

;; 7
(define (memo x l out)
  (conde
    [(nullo l) u#]
    [(eq-caro l x) (== l out)]
    [else
      (fresh (d)
        (cdro l d)
        (memo x d out))]))

;; 8
;; (mem x (cdr l))

;; 10
(check-run 1 (out)
           (memo 'tofu '(a b tofu d tofu e) out)
           => '((tofu d tofu e)))

;; 11
(check-run 1 (out)
           (fresh (x)
             (memo 'tofu `(a b ,x d tofu e) out))
           => '((tofu d tofu e)))

;; 12
(check-run* (r)
            (memo r
                  '(a b tofu d tofu e)
                  '(tofu d tofu e))
            => '(tofu))

;; 13
(check-run* (q)
            (memo 'tofu '(tofu e) '(tofu e))
            (== #t q)
            => '(#t))

;; 14
(check-run* (q)
            (memo 'tofu '(tofu e) '(tofu))
            (== #t q)
            => '())

;; 15
(check-run* (x)
           (memo 'tofu '(tofu e) `(,x e))
            => '(tofu))

;; 16
(check-run* (x)
           (memo 'tofu '(tofu e) '(peas e))
            => '())

;; 17
(check-run* (out)
            (fresh (x)
              (memo 'tofu `(a b ,x d tofu e) out))
            => '((tofu d tofu e) (tofu e)))

;; 18
(check-run 12 (z)
           (fresh (u)
             (memo 'tofu `(a b tofu d tofu e . ,z) u))
           => '(_.0
                 _.0
                 (tofu . _.0)
                 (_.0 tofu . _.1)
                 (_.0 _.1 tofu . _.2)
                 (_.0 _.1 _.2 tofu . _.3)
                 (_.0 _.1 _.2 _.3 tofu . _.4)
                 (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
                 (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
                 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
                 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
                 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9)))

;; 19
;; finding the first two tofus

;; 20
;; the remaining z list which could contain tofu

;; 21
;; we don't need the nullo or the else
(define (memo2 x l out)
  (conde
    [(eq-caro l x) (== l out)]
    [(fresh (d)
      (cdro l d)
      (memo2 x d out))]))

(check-run* (out)
            (memo2 'tofu '(a b tofu d tofu e) out)
            => '((tofu d tofu e) (tofu e)))

;; 22
(define (rember x l)
  (cond
    [(null? l) '()]
    [(eq-car? l x) (cdr l)]
    [else
      (cons (car l)
            (rember x (cdr l)))]))

;; 23
(check-equal? (rember 'peas '(a b peas d peas e))
              '(a b d peas e))

;; 24
(define (rembero1 x l out)
  (conde
    [(nullo l) (== '() out)]
    [(eq-caro l x) (cdro l out)]
    [else
      (fresh (res)
        (fresh (d)
          (cdro l d)
          (rembero1 x d res))
        (fresh (a)
          (caro l a)
          (cons a res out)))]))

;; 25
;; we need both freshes for the cons as well as rembero

;; 26
(define (rembero2 x l out)
  (conde
    [(nullo l) (== '() out)]
    [(eq-caro l x) (cdro l out)]
    [else
      (fresh (a d res)
        (cdro l d)
        (rembero2 x d res)
        (caro l a)
        (conso a res out))]))

;; 27
(define (rembero x l out)
  (conde
    [(nullo l) (== '() out)]
    [(eq-caro l x) (cdro l out)]
    [else
      (fresh (a d res)
        (conso a d l)
        (rembero x d res)
        (conso a res out))]))

;; 30

(define-syntax-rule (test-rembero rembero)
  (check-run 1 (out)
             (fresh (y)
               (rembero 'peas `(a b ,y d peas e) out))
             => '((a b d peas e))))

;; fails? (test-rembero rembero1)
(test-rembero rembero2)
(test-rembero rembero)

;; 31
(check-run* (out)
            (fresh (y z)
              (rembero y `(a b ,y d ,z e) out))
            => '((b a d _.0 e)
                 (a b d _.0 e)
                 (a b d _.0 e)
                 (a b d _.0 e)
                 (a b _.0 d e)
                 (a b e d _.0)
                 (a b _.0 d _.1 e)))

;; 32
;; (== y 'a)

;; 33
;; b becomes first as the first as is removed

;; 34
;; since y unifies with a, it replaces the y in the list with a

;; 35
;; (== y 'b)

;; 36
;; the first b is removed, but y is replaced with b in the list

;; 38
;; no, now (== y 'd))

;; 41
;; the first d is now removed and the second d remains

;; 42
;; now z has been removed from the list

;; 43
;; because (== y z))

;; 44
;; (== y 'e))

;; 45
;; (== z a)

;; 46
;; (=!= z y)

;; 47
;; We don't remove anything from the list

;; 49
(check-run* (r)
            (fresh (y z)
              (rembero y `(,y d ,z e) `(,y d e))
              (== `(,y ,z) r))
            => '((d d)
                 (d d)
                 (_.0 _.0)
                 (e e)))

;; 50
(check-run 1 (q)
            (rembero 'd '(d d d e) '(d d e))
            (== #t q)
            => '(#t))

;; 51
(check-run 2 (q)
            (rembero 'd '(d d d e) '(d d e))
            (== #t q)
            => '(#t #t))

;; 55
(check-run* (q)
            (rembero 'x '(x d x e) '(x d e))
            (== #t q)
            => '(#t))

;; 56
(check-run 1 (q)
            (rembero 'e '(e d e e) '(e d e))
            (== #t q)
            => '(#t))

;; 57
(check-run 13 (w)
           (fresh (y z out)
             (rembero y `(a b ,y d ,z . ,w) out))
           => '(_.0
                _.0
                _.0
                _.0
                _.0
                ()
                (_.0 . _.1)
                (_.0)
                (_.0 _.1 . _.2)
                (_.0 _.1)
                (_.0 _.1 _.2 . _.3)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3 . _.4)))

;; o_O

;; 58
(check-run 1 (w)
           (rembero 'a `(a b a d . ,w) `(b a d . ,w))
           => '(_.0))

;; 60
(check-run 1 (w)
           (rembero 'b `(a b b d . ,w) `(a b d . ,w))
           => '(_.0))

(check-run 1 (w)
           (rembero 'd `(a b d d . ,w) `(a b d . ,w))
           => '(_.0))

;; 61
(check-run 2 (w)
           (rembero 'd `(a b d d . ,w) `(a b d . ,w))
           => '(_.0 _.0))

;; 62
(check-run 1 (w)
           (rembero 'q `(a b a d . ,w) `(a b a d . ,w))
           => '(()))

;; helper to explain why an answer is provided.
(define (whyo w1 v)
  (fresh (y z out w)
    (rembero y `(a b ,y d ,z . ,w) out)
    (== w w1)
    (== v `((y ,y)
            (z ,z)
            (out ,out)))))

(check-run 1 (v)
           (whyo '(_.0) v)
           => '(((y a)
                 (z _.0)
                 (out (b a d _.0 _.0)))))

;; 63
(check-run 1 (v)
           (whyo '(_.0 . _.1) v)
           => '(((y a)
                 (z _.0)
                 (out (b a d _.0 _.0 . _.1)))))

;; 64
(check-run 1 (v)
           (whyo '(_.0) v)
           => '(((y a)
                 (z _.0)
                 (out (b a d _.0 _.0)))))

;; 65
(check-run 1 (v)
           (whyo '(_.0 _.1 . _.2) v)
           => '(((y a)
                 (z _.0)
                 (out (b a d _.0 _.0 _.1 . _.2)))))

;; 68
(define (surpriseo s)
  (rembero s '(a b c) '(a b c)))

;; 69
(check-run* (r)
            (== 'd r)
            (surpriseo r)
            => '(d))

;; 70
(check-run* (r)
            (surpriseo r)
            => '(_.0))

;; 71
(check-run* (r)
            (surpriseo r)
            (== 'b r)
            => '(b))

;; but, but b. b!! bah.

;; screw carrots, I need booze.
