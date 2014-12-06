#lang racket
(require test-engine/racket-tests) ; <======= needed for testing

; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))
(check-expect (atom? (quote())) #f) ; make sure atom? is defined #=> should output #f

; ########### PRACTICE DESIGNING RECURSIVE FUNCTIONS ################
; Try to write each function from scratch instead of following the book
; Make sure you wire your brain by practicing && writing all of them

(define member? ; <--------------------- MEMBER?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or
         (eq? a (car lat))
         (member? a (cdr lat)))))))

(check-expect (member? 2 '(1 2 3 4 5)) #t)
(check-expect (member? 2 '(1 3 4 5))   #f)

(define rember ; <--------------------- REMBER
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(check-expect (rember 2 '(1 2 3 4)) '(1 3 4))
(check-expect (rember 2 '(1 3 4))   '(1 3 4))
(check-expect (rember 4 '(1 2 3 4)) '(1 2 3))
(check-expect (rember 1 '(1 2 3 4)) '(2 3 4))

(define first ; <--------------------- FIRST
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else (cons (car (car lat)) (first (cdr lat)))))))

(check-expect (first '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))
              '(apple plum grape bean))
(check-expect (first '()) '())

(define insertR ; <--------------------- INSERT_R
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons old (cons new (cdr lat))))
         (else (cons (car lat) (insertR new old (cdr lat)))))))))

(check-expect (insertR 'x 2 '(1 2 3 4)) '(1 2 x 3 4))
(check-expect (insertR 9 1 '(1)) '(1 9))
(check-expect (insertR 9 1 '(2 3 4 5 6)) '(2 3 4 5 6))

(define insertL ; <--------------------- INSERT_L
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons new lat))
         (else (cons (car lat) (insertL new old (cdr lat)))))))))

(check-expect (insertL 'x 2 '(1 2 3 4)) '(1 x 2 3 4))
(check-expect (insertL 9 1 '(1)) '(9 1))
(check-expect (insertL 9 1 '(2 3 4 5 6)) '(2 3 4 5 6))

(define subst ; <--------------------- SUBST
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons new (cdr lat)))
         (else (cons (car lat) (subst new old (cdr lat)))))))))

(check-expect (subst 'x 2 '(1 2 3 4)) '(1 x 3 4))
(check-expect (subst 9 1 '(1)) '(9))
(check-expect (subst 9 1 '(2 3 4 5 6)) '(2 3 4 5 6))

(define subst2 ; <--------------------- SUBST2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
         (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(check-expect (subst2 'x 2 3 '(1 2 3 4)) '(1 x 3 4))
(check-expect (subst2 9 0 1 '(1)) '(9))
(check-expect (subst2 9 0 1 '(2 3 4 5 6)) '(2 3 4 5 6))

(define multirember ; <--------------------- MULTIREMBER
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))

(check-expect (multirember 2 '(1 2 3 2 4 2)) '(1 3 4))
(check-expect (multirember 1 '(1 1 1 1)) '())

(define multinsertR ; <--------------------- MULTI_INSERT_R
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons old (cons new (multinsertR new old (cdr lat)))))
         (else (cons (car lat) (multinsertR new old (cdr lat)))))))))

(check-expect (multinsertR 2 1 '(1 1 1)) '(1 2 1 2 1 2))
(check-expect (multinsertR 2 1 '(0 1 2)) '(0 1 2 2))

(define multinsertL ; <--------------------- MULTI_INSERT_L
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons new (cons old (multinsertL new old (cdr lat)))))
         (else (cons (car lat) (multinsertL new old (cdr lat)))))))))

(check-expect (multinsertL 2 1 '(1 1 1)) '(2 1 2 1 2 1))
(check-expect (multinsertL 2 1 '(0 1 2)) '(0 2 1 2))

(define multisubst ; <--------------------- MULTI_SUBST
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
         (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(check-expect (multisubst 2 1 '(1 1 1)) '(2 2 2))
(check-expect (multisubst 2 1 '(1 2 3 2 1)) '(2 2 3 2 2))


; I think I've got it!

(test) ; <======= needed for testing
