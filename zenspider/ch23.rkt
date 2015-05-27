#lang racket/base

(require "lib/reasonable.rkt")
(require "ch22.rkt")

(provide eq-car°)

(define (list° l)                       ; 5
  (cond-e [(null° l) %s]
          [(pair° l) (fresh (d)
                       (cdr° l d)
                       (list° d))]))

(define (lol° l)                        ; 17
  (cond-e [(null° l) %s]
          [(fresh (a)
             (car° l a)
             (list° a))
           (fresh (d)
             (cdr° l d)
             (lol° d))]))

(define (twins1° s)                     ; 31
  (fresh (x y)
    (cons° x y s)
    (cons° x '() y)))

(define (twins2° s)                     ; 36 -- my first was MUCH too complicated
  (fresh (x)
    (≈ `(,x ,x) s)))

(define twins° twins2°)

(define (lot1° l)                       ; 37
  (cond-e [(null° l) %s]
          [(fresh (a)
             (car° l a)
             (twins° a))
           (fresh (d)
             (cdr° l d)
             (lot° d))]))

(define (listof° pred° l)               ; 48
  (cond-e [(null° l) %s]
          [(fresh (a)
             (car° l a)
             (pred° a))
           (fresh (d)
             (cdr° l d)
             (listof° pred° d))]))

(define (lot2° l)                       ; 50
  (listof° twins° l))

(define lot° lot2°)

(define (eq-car° l x)                   ; 54
  (car° l x))

(define (member° x l)                   ; 54
  (cond-e [(eq-car° l x)]
          [else (fresh (d)
                  (cdr° l d)
                  (member° x d))]))

(define (pmember1° x l)
  (cond-e [(eq-car° l x) (cdr° l '())]
          [else (fresh (d)
                  (cdr° l d)
                  (pmember1° x d))]))

(define (pmember2° x l)
  (cond-e [(eq-car° l x) (cdr° l '())]
          [(eq-car° l x)]
          [else (fresh (d)
                  (cdr° l d)
                  (pmember2° x d))]))

(define (pmember3° x l)
  (cond-e [(eq-car° l x) (cdr° l '())]
          [(eq-car° l x) (fresh (a d)
                           (cdr° l (cons a d)))]
          [else (fresh (d)
                  (cdr° l d)
                  (pmember3° x d))]))

(define (pmember4° x l)
  (cond-e [(eq-car° l x) (fresh (a d)
                           (cdr° l (cons a d)))]
          [(eq-car° l x) (cdr° l '())]
          [else (fresh (d)
                  (cdr° l d)
                  (pmember4° x d))]))

(define pmember° pmember4°)

;;; The First Commandment
;;
;; To transform a function whose value is a boolean into a functions
;; whose value is a goal, replace cond with cond-e and unnest each
;; question and answer. Unnest the answer #t (or #f) by replacing it
;; with %s (or %u).

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-run* (x)                       ; 8
              (list° `(a b ,x d))
              => '(_.0))

  (check-run1 (x)                       ; 10
              (list° `(a b c . ,x))
              => '(()))

  (check-run 5 (x)                      ; 14
             (list° `(a b c . ,x))
             => '(()
                  (_.0)
                  (_.0 _.1)
                  (_.0 _.1 _.2)
                  (_.0 _.1 _.2 _.3)))

  (check-run1 (l)                       ; 20
              (lol° l)
              => '(()))

  (check-run* (q)                       ; 21
              (fresh (x y)
                (lol° `((a b) (,x c) (d ,y)))
                (≈ #t q))
              => '(#t))

  (check-run1 (q)                       ; 22
              (fresh (x)
                (lol° `((a b) . ,x))
                (≈ #t q))
              => '(#t))

  (check-run1 (x)                       ; 23
              (lol° `((a b) (c d) . ,x))
              => '(()))

  (check-run 5 (x)                      ; 24
             (lol° `((a b) (c d) . ,x))
             => '((  )
                  (())
                  (() ())
                  (() () ())
                  (() () () ())))

  (check-run* (q)                       ; 32
              (twins° '(tofu tofu))
              (≈ #t q)
              => '(#t))

  (check-run* (z)                       ; 33
              (twins° `(,z tofu))
              => '(tofu))

  (check-run1 (z)                       ; 38
              (lot° `((g g) . ,z))
              => '(()))

  (check-run 5 (z)                      ; 42
             (lot° `((g g) . ,z))
             => '((  )
                  ((_.0 _.0))
                  ((_.0 _.0) (_.1 _.1))
                  ((_.0 _.0) (_.1 _.1) (_.2 _.2))
                  ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))))


  (check-run 5 (r)                      ; 45
             (fresh (w x y z)
               (lot° `((g g) (e ,w) (,x ,y) . ,z))
               (≈ `(,w (,x ,y) ,z) r))
             => '((e (_.0 _.0) ())
                  (e (_.0 _.0) ((_.1 _.1)))
                  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
                  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
                  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))))

  (check-run 3 (r)                      ; 47
             (fresh (w x y z)
               (≈ `((g g) (e ,w) (,x ,y) . ,z) r)
               (lot° r))
             => '(((g g) (e e) (_.0 _.0))
                  ((g g) (e e) (_.0 _.0) (_.1 _.1))
                  ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

  (check-run 3 (out)                    ; 49
             (fresh (w x y z)
               (≈ `((g g) (e ,w) (,x ,y) . ,z) out)
               (listof° twins° out))
             => '(((g g) (e e) (_.0 _.0))
                  ((g g) (e e) (_.0 _.0) (_.1 _.1))
                  ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

  (check-run* (q)                       ; 57
              (member° 'olive '(virgin olive oil))
              (≈ #t q)
              => '(#t))

  (check-run1 (y)                       ; 58
              (member° y '(hummus with pita))
              => '(hummus))

  (check-run1 (y)                       ; 59
              (member° y '(with pita))
              => '(with))

  (check-run1 (y)                       ; 60
              (member° y '(pita))
              => '(pita))

  (check-run* (y)                       ; 61
              (member° y '())
              => '())

  (check-run 5 (y)
             (member° y '(hummus with pita))
             => '(hummus with pita))

  (define (identity l)                  ; 65
    (run* (y)
          (member° y l)))

  (check-run* (x)                       ; 66
              (member° 'e `(pasta ,x fagioli))
              => '(e))

  (check-run1 (x)                       ; 69
              (member° 'e `(pasta e ,x fagioli))
              => '(_.0))

  (check-run1 (x)                       ; 70
              (member° 'e `(pasta ,x e fagioli))
              => '(e))

  (check-run* (r)                       ; 71
              (fresh (x y)
                (member° 'e `(pasta ,x fagioli ,y))
                (≈ `(,x ,y) r))
              => '((e _.0)
                   (_.0 e)))

  (check-run1 (l)                       ; 73
              (member° 'tofu l)
              => '((tofu . _.0)))

  (check-run 5 (l)                      ; 76
             (member° 'tofu l)
             => '((tofu . _.0)
                  (_.0 tofu . _.1)
                  (_.0 _.1 tofu . _.2)
                  (_.0 _.1 _.2 tofu . _.3)
                  (_.0 _.1 _.2 _.3 tofu . _.4)))

  (check-run 5 (l)                      ; 80
             (pmember1° 'tofu l)
             => '((tofu)
                  (_.0 tofu)
                  (_.0 _.1 tofu)
                  (_.0 _.1 _.2 tofu)
                  (_.0 _.1 _.2 _.3 tofu)))

  (check-run* (q)                       ; 84
              (pmember2° 'tofu '(a b tofu d tofu))
              (≈ #t q)
              => '(#t #t #t))

  (check-run* (q)                       ; 88
              (pmember3° 'tofu '(a b tofu d tofu))
              (≈ #t q)
              => '(#t #t))

  (check-run 4 (l)                      ; 89
             (pmember3° 'tofu l)
             => '((tofu)
                  (tofu _.0 . _.1)
                  (_.0 tofu)
                  (_.0 tofu _.1 . _.2)))

  (check-run 4 (l)                      ; 94
             (pmember4° 'tofu l)
             => '((tofu _.0 . _.1)
                  (tofu)
                  (_.0 tofu _.1 . _.2)
                  (_.0 tofu)))

  (define (memberrev° x l)              ; 98
    (cond-e [(fresh (d)
               (cdr° l d)
               (memberrev° x d))]
            [(eq-car° l x)]))

  (check-run* (x)                       ; 100
              (memberrev° x '(pasta e fagioli))
              => '(fagioli e pasta))

  (define (reverse-list l)              ; 101
    (run* (y)
          (memberrev° y l)))

  (check-equal? (reverse-list '(pasta e fagioli))
                '(fagioli e pasta)))
