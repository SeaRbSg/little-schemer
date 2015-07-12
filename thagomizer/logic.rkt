#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")

(define memberrevo
  (lambda (x l)
    (conde
     [s# (fresh (d)
                (cdro l d)
                (memberrevo x d))]
     [else (eq-caro l x)])))

(define onceo
  (lambda (g)
    (condu
     [g s#]
     [else u#])))

(define membero
  (lambda (x l)
    (conde
     [(eq-caro l x) s#]
     [(nullo '())
      (fresh (d)
             (cdro l d)
             (membero x d))])))


(define distincto
  (lambda (l)
    (conde
     [(nullo l) s#]
     [else
      (fresh (head tail)
             (conso head tail l)
             (conda
              [(membero head tail) u#]
              [(distincto tail) s#]))])))

(test-case "distincto"
           [check-equal? (run* (r)
                               (distincto '())
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a b c))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (distincto '(a a))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (distincto '(a b a))
                               (== #t r))
                         '()])


(define higher
  (lambda (upper lower condo_list)
    (condu
     [(nullo condo_list) u#]
     [(eq-caro condo_list upper) s#]
     [(eq-caro condo_list lower) u#]
     [else
      (fresh (new_list)
             (cdro condo_list new_list)
             (higher upper lower new_list))])))


(test-case "higher"
           [check-equal? (run* (r)
                               (higher 'a 'b '(a b))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (higher 'a 'b '(b a))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (higher 'a 'b '(e f a b))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (higher 'a 'b '(e f b a))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (higher 'a 'b '())
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (higher 'a 'b '(a c b))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (higher 'a 'b '(b e a))
                               (== #t r))
                         '()])

(define not-adjacent
  (lambda (x y l)
    (conde
     [(nullo l) u#]
     [else
      (fresh (kar kdr kadr kddr)
             (conso kar kdr l)
             (conso kadr kddr kdr)
             (conde
              [(== x kar) (membero y kddr)]
              [(== y kar) (membero x kddr)]
              [else
               (not-adjacent x y kdr)]))])))

(test-case "not-adjacent"
           [check-equal? (run* (r)
                               (not-adjacent 'a 'b '(a b))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (not-adjacent 'a 'b '(b a))
                               (== #t r))
                         '()]
           [check-equal? (run* (r)
                               (not-adjacent 'a 'b '(a f b))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (not-adjacent 'a 'b '(b f a))
                               (== #t r))
                         '(#t)]
           [check-equal? (run* (r)
                               (not-adjacent 'a 'b '())
                               (== #t r))
                         '()])

(define logic
  (lambda (l)
    (fresh (funf vier drei zwei eins)
           (all
            ;; Adam does not live on the top
            (membero 'A `(,vier ,drei ,zwei ,eins))
            ;; Bill does not live on the bottom
            (membero 'B `(,funf ,vier ,drei ,zwei))
            ;; Cora lives in the middle
            (membero 'C `(,vier ,drei ,zwei))
            ;; Dale lives on a higher floor than does Bill
            (higher 'D 'B `(,funf ,vier ,drei ,zwei ,eins))
            ;; Erin does not live on a floor adjacent to Cora's
            (not-adjacent 'C 'E `(,funf ,vier ,drei ,zwei ,eins))
            ;; Cora does not live on a floor adjacent to Bill's
            (not-adjacent 'C 'B `(,funf ,vier ,drei ,zwei ,eins)))
           (== l `(,funf ,vier ,drei ,zwei ,eins)))))

[check-equal? (run* (r)
                    (logic r))
              '((D C A B E))]


;; 1. Adam does not live on the top floor.
;; 2. Bill does not live on the bottom floor.
;; 3. Cora does not live on either the top or the bottom floor.
;; 4. Dale lives on a higher floor than does Bill.
;; 5. Erin does not live on a floor adjacent to Cora's.
;; 6. Cora does not live on a floor adjacent to Bill's.


;;; Problem of my choice


;; 1. The rocket that will launch in February is made by Rubicorp.
;; 2. The rocket that will launch in March is made by Permias.
;; 3. The rocket developed by Omnipax will launch 1 month before the Dreadco.
;; 4. The Athios will launch 1 month before the Cornick.

;; Names: Athios, Cornick, Dreadco, Foltron
;; Companies: Omnipax, Permias, Rubicorp, Vexatech
;; Months: January, February, March, April

;; Correct answer:
;; Months	  Names	   Companies
;; January	  Foltron  Omnipax
;; February       Dreadco  Rubicorp
;; March	  Athios   Permias
;; April	  Cornick  Vexatech



(define one-month-before
  (lambda (x y l)
    (conde
     [(nullo l) u#]
     [else
      (fresh (kar kdr kadr kddr)
             (conso kar kdr l)
             (conso kadr kddr kdr)
             (conde
              [(== x kar) (== y kadr)]
              [else
               (one-month-before x y kdr)]))])))

;; The rocket developed by Omnipax will launch 1 month before the Dreadco.
(define clause-three
  (lambda (cx ny l)
    (conde
     [(nullo l) u#]
     [else
      (fresh (kar kdr kadr kddr karn karc kadrn kadrc)
             (conso kar kdr l)
             (conso kadr kddr kdr)
             (conso karn karc kar)
             (conso kadrn kadrc kadr)
             (conde
              [(eq-caro karc cx) (== kadrn ny)]
              [else
               (clause-three cx ny kdr)]))])))

(define my-logic
  (lambda (l)
    (fresh (nj nf nm na cj cf cm ca)
           (all
            (== cf 'Rubicorp)
            (== cm 'Permias)
            (clause-three 'Omnipax 'Dreadco `((,nj ,cj) (,nf ,cf) (,nm ,cm) (,na ,ca)))
            (one-month-before 'Athios 'Cornick `(,nj ,nf ,nm ,na))
            (membero 'Folton `(,nj ,nf ,nm ,na))
            (membero 'Vexatech `(,cj ,cf ,cm ,ca)))
           (== `((,nj ,cj) (,nf ,cf) (,nm ,cm) (,na ,ca)) l))))

[check-equal? (run* (r)
                    (my-logic r))
              '(((Folton Omnipax) (Dreadco Rubicorp) (Athios Permias) (Cornick Vexatech)))]



