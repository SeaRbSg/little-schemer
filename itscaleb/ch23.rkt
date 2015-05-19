#lang racket
(require "../lib/mk.rkt")
(require "./reasoned.rkt")
(require rackunit)

(define twinso
  (lambda (s)
    (fresh (x)
           (== `(,x ,x) s))))

(define listofo
  (lambda (predo l)
    (conde
     ((nullo l) s#)
     ((fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d)))
     (else u#))))

(define loto
  (lambda (l)
    (listofo twinso l)))

(define membero
  (lambda (x l)
    (conde
     ((nullo l) u#)
     ((eq-caro l x) s#)
     (else (fresh (d)
                  (cdro l d)
                  (membero x d))))))

(define eq-caro
  (lambda (l x)
    (caro l x)))

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

(check-equal? (run 3 (out)
                   (fresh (w x y z)
                          (== `((g g) (e ,w) (,x ,y) . ,z) out)
                          (loto out)))
              '(((g g) (e e) (_.0 _.0))
                ((g g) (e e) (_.0 _.0) (_.1 _.1))
                ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

(check-equal? (run 1 (y)
                   (membero y '(hummus with pita)))
              '(hummus))

(check-equal? (run* (y)
                    (membero y '(hummus with pita)))
              '(hummus with pita))

(check-equal?  (run* (x)
                     (membero 'e `(pasta e ,x fagioli)))
               '(_.0 e))

(check-equal? (run* (r)
                    (fresh (x y)
                           (membero 'e `(pasta ,x fagioli ,y))
                           (== `(,x ,y) r)))
              '((e _.0) (_.0 e)))


