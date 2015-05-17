#lang racket
(require "../lib/mk.rkt")
(require "./reasoned.rkt")
(require rackunit)

(define twinso
  (lambda (s)
    (fresh (x)
           (== `(,x ,x) s))))

(define loto
  (lambda (l)
    (conde
     ((nullo l) s#)
     ((fresh (a)
             (caro l a)
             (twinso a))
      (fresh (d)
             (cdro l d)
             (loto d)))
     (else u#))))

(check-equal? (run* (z)
                    (twinso `(,z tofu)))
              '(tofu))

(check-equal? (run* (z)
                    (twinso '(tofu tofu))
                    (== #t z))
              '(#t))

(check-equal? (run 1 (z)
                    (loto `((g g) . ,z)))
              '(()))
