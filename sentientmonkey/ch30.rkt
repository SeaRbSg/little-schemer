#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch21.rkt")
(require "ch26.rkt")
(require "ch27.rkt")

;; 1
(check-run* (r)
           (conda
             (u# s#)
             (else u#))
           => '())
;; 2
(check-run* (r)
            (conda
              (u# s#)
              (else s#))
            => '(_.0))

;; 3
(check-run* (r)
            (conda
              (s# u#)
              (else u#))
            => '())

;; 4
(check-run* (r)
            (conda
              (s# s#)
              (else u#))
            => '(_.0))

;; 5
(check-run* (x)
            (conda
              [(== 'olive x) s#]
              [(== 'oil x) s#]
              [else u#])
            => '(olive))

;; 7
(check-run* (x)
            (conda
              [(== 'virgin x) u#]
              [(== 'olive x) s#]
              [(== 'oil x) s#]
              [else u#])
            => '())

;; 8
(check-run* (q)
            (fresh (x y)
              (== 'split x)
              (== 'pea y)
              (conda
                [(== 'split x) (== x y)]
                [else s#])
              (== #t q))
            => '())

;; 9
(check-run* (q)
            (fresh (x y)
              (== 'split x)
              (== 'pea y)
              (conda
                [(== x y) (== 'split x)]
                [else s#])
              (== #t q))
            => '(#t))

;; 11
(define (not-pastao x)
  (conda
    [(== 'pasta x) u#]
    [else s#]))

(check-run* (x)
            (conda
              [(not-pastao x) u#]
              [else (== 'spaghetti x)])
            => '(spaghetti))

;; 12
(check-run* (x)
            (== 'spaghetti x)
            (conda
              [(not-pastao x) u#]
              [else (== 'spaghetti x)])
            => '())

;; 13
; (run* (q)
;   (condu
;     (alwayso s#)
;     (else u#))
;   (== #t q))
;; no answer...

;; 14
(check-run* (q)
            (condu
              (alwayso s#)
              (else u#))
            (== #t q)
            => '(#t))

;; 15
; (run* (q)
;   (condu
;     (s# alwayso)
;     (else u#))
;   (== #t q))
;; no answer...

;; 16
;; u == uni-

;; 17
; (run 1 (q)
;   (conda
;     (alwayso s#)
;     (else u#))
;   u#
;   (== #t q))
;; no answer...

;; 18
(check-run 1 (q)
           (condu
             (alwayso s#)
             (else u#))
           u#
           (== #t q)
           => '())

;; 19
(define (onceo g)
  (condu
    (g s#)
    (else u#)))

(check-run* (x)
            (onceo (teacupo x))
            => '(tea))

;; 20
(check-run 1 (q)
           (onceo (salo nevero))
           u#
           => '())

;; 21
(check-run* (r)
            (conde
              [(teacupo r) s#]
              [(== #f r) s#]
              [else u#])
            => '(tea cup #f))

;; 22
(check-run* (r)
            (conda
              [(teacupo r) s#]
              [(== #f r) s#]
              [else u#])
            => '(tea cup))

;; 23
(check-run* (r)
            (== #f r)
            (conda
              [(teacupo r) s#]
              [(== #f r) s#]
              [else u#])
            => '(#f))

;; 24
(check-run* (r)
            (== #f r)
            (condu
              [(teacupo r) s#]
              [(== #f r) s#]
              [else u#])
            => '(#f))

;; 26
(define (bumpo n x)
  (conde
    [(== n x) s#]
    [else
      (fresh (m)
        (-o n '(1) m)
        (bumpo m x))]))

(check-run* (x)
            (bumpo '(1 1 1) x)
            => '((1 1 1)
                 (0 1 1)
                 (1 0 1)
                 (0 0 1)
                 (1 1)
                 (0 1)
                 (1)
                 ()))

;; 27
(define (gen&testo op i j k)
  (onceo
    (fresh (x y z)
      (op x y z)
      (== i x)
      (== j y)
      (== k z))))

(check-run* (q)
            (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
            (== #t q)
            => '(#t))
;; 28
(check-run* (q)
            (fresh (x y z)
              (gen&testo +o x y z)
              (== `(,x ,y ,z) q))
            => '((_.0 () _.0)))

;; 41
; (run 1 (q)
;   (gen&testo +o '(0 0 1) '(1 1) '(0 1 1)))
;; no answer...

;; 43
(define (enumerateo op r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (gen&testo op i j k)
    (== `(,i ,j ,k) r)))

(check-run* (s)
            (enumerateo +o s '(1 1))
            => '(((1 1) (1 1) (0 1 1))
                 ((1 1) (0 1) (1 0 1))
                 ((1 1) (1) (0 0 1))
                 ((1 1) () (1 1))
                 ((0 1) (1 1) (1 0 1))
                 ((0 1) (0 1) (0 0 1))
                 ((0 1) (1) (1 1))
                 ((0 1) () (0 1))
                 ((1) (1 1) (0 0 1))
                 ((1) (0 1) (1 1))
                 ((1) (1) (0 1))
                 ((1) () (1))
                 (() (1 1) (1 1))
                 (() (0 1) (0 1))
                 (() (1) (1))
                 (() () ())))

;; 56
(check-run 1 (s)
           (enumerateo +o s '(1 1 1))
           => '(((1 1 1) (1 1 1) (0 1 1 1))))

;; 57
(define (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (all
      (full-addero d a b c e)
      (addero e x y z))))
