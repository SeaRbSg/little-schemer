#lang racket

(require "lib/shared.rkt")
(require rackunit)

; Difference between eq? and equal? in Racket
; The eq? operator compares two values, returning #t when the values refer to the same object. 
; In some cases, however, eq? is unsuitable as a comparison operator, because the generation of objects is not clearly defined.
; In particular, two applications of + to the same two exact integers may or may not produce results that are eq?, 
; although the results are always equal?. <=============== THE KEY!!
; The behavior of a datatype with respect to eq? is generally specified with the datatype and its associated procedures.
; Summary: for most purposes both are analogous.

(module+ test
  (check-true (eq? 2 '2))               ; atom and number are equal && eq
  (check-true (equal? 2 '2))
  (check-true (eq? (+ 1 1) '2))         ; indeed!
  (check-true (equal? (+ 1 1) '2))
  (check-false (eq? '(2) '2))           ; at least there is still some decency around lists
  (check-true (eq? (null? '()) #t))     ; a truth is a truth is a truth
  (check-true (equal? (null? '()) #t)))

; Idiosyncracies of numbers and atoms in Racket

(module+ test
  (check-true (atom? 2))     ;  2 is an atom and a number
  (check-true (number? 2))
  (check-true (atom? '2))    ; '2 is an atom, and yes, also a number
  (check-true (number? '2)))

(define rember*
  (lambda (a las) ; I dont like l, rather prefer las as list of S_ex(pressions), like lat as list of atoms
    (cond
      ((null? las) las)
      ((atom? (car las))
        (cond
          ((eq? a (car las)) (rember* a (cdr las)))
          (else (cons (car las) (rember* a (cdr las))))))
      (else (cons (rember* a (car las)) (rember* a (cdr las))))))) ; HOLY GUACAMOLE

(module+ test
  (check-equal? (rember* 1 '((1 2 3) 1)) '((2 3)))
  (check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
                              '((coffee) ((tea)) (and (hick))))
  (check-equal? (rember* 'sauce '(((tomate sauce)) ((bean) sauce) (and ((flying)) sauce)))
                                '(((tomate)) ((bean)) (and ((flying))))))

(define lat?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((atom? (car lat)) (lat? (cdr lat)))
         (else #f))))))

(module+ test
  (check-false (lat? '((1 2 3) 1)))
  (check-true  (lat? '(1 2 3 1)))
  (check-false (lat? '(1 2 ((3)) 1))))

(define insertR*
  (lambda (new old las)
    (cond
      ((null? las) las)
      ((atom? (car las))
       (cond
         ((eq? old (car las)) (cons old (cons new (insertR* new old (cdr las)))))
         (else (cons (car las) (insertR* new old (cdr las))))))
       (else (cons (insertR* new old (car las)) (insertR* new old (cdr las)))))))

(module+ test
  (check-equal?
    (insertR* 'roast 'chuck '(((how much (wood)) could ((a wood) chuck))
                              (((chuck)))
                              (if (a) ((wood chuck)))
                              could chuck wood))
                            '(((how much (wood)) could ((a wood) chuck roast))
                              (((chuck roast)))
                              (if (a) ((wood chuck roast)))
                              could chuck roast wood)))

; FIRST COMMANDMENT
; When recurring:
;   - lat => ask (null? lat) && (else
;   - n   => ask (zero? n) && (else
;   - las => ask (null? las) && (atom? (car las)) & (else

; FOURTH COMMANDMENT
; Always change on or more arguments when recurring. For:
;   - lat => use (cdr lat)
;   - n   => use (sub1 n)
;   - las => use both (car las) (cdr las) whenever possible => ((null? las) == #f && (atom? (car las)) == #f)
;
; Test values for termination:
;   - cdr  => null?
;   - sub1 => zero?

(define occur*
  (lambda (a las)
    (cond
      ((null? las) 0)                                    ; question 1
      ((atom? (car las))                                 ; question 2
       (cond
         ((eq? a (car las)) (add1 (occur* a (cdr las)))) ; counts and adds1 only in single place!!
         (else (occur* a (cdr las)))))
      (else                                              ; question 3
       (+ (occur* a (car las)) (occur* a (cdr las))))))) ; recurring on car and cdr

(module+ test
  (check-equal? (occur* 1 '(1 2 3 (1 (2) ((3 4 1 (1 1)))))) 5)
  (check-equal? (occur* 1 '(1 1 1 1 2)) 4))

(define subst*
  (lambda (new old las)
    (cond
      ((null? las) las)
      ((atom? (car las))
       (cond
         ((eq? old (car las)) (cons new (subst* new old (cdr las))))
         (else (cons (car las) (subst* new old (cdr las))))))
      (else (cons (subst* new old (car las)) (subst* new old (cdr las)))))))

(module+ test
  (check-equal? (subst* 0 1 '(1 2 3 (1 (2) ((3 4 1 (1 1))))))
                            '(0 2 3 (0 (2) ((3 4 0 (0 0)))))))

(define insertL*
  (lambda (new old las)
    (cond
      ((null? las) las)
      ((atom? (car las))
       (cond
         ((eq? old (car las)) (cons new (cons old (insertL* new old (cdr las))))) ; again, new+old insertion happens only here!!
         (else (cons (car las) (insertL* new old (cdr las))))))
      (else (cons (insertL* new old (car las)) (insertL* new old (cdr las)))))))

(module+ test
  (check-equal?
    (insertL* 'pecker 'chuck '(((how much (wood)) could ((a wood) chuck))
                               (((chuck)))
                               (if (a) ((wood chuck))) could chuck wood))
    '(((how much (wood)) could ((a wood) pecker chuck))
      (((pecker chuck)))
      (if (a) ((wood pecker chuck))) could pecker chuck wood)))


(define member*
  (lambda (a las)
    (cond
      ((null? las) #f)
      ((atom? (car las))
       (cond
         ((eq? a (car las)) #t)
         (else (member* a (cdr las)))))  ; can be simplified with (or !!
      (else (or (member* a (car las)) (member* a (cdr las)))))))

(module+ test
  (check-true  (member* 'chips '((potato) (chips ((with) fish) (chips)))))
  (check-false (member* 'chump '((potato) (chips ((with) fish) (chips))))))

(define leftmost
  (lambda (las)
    (cond
      ((null? las) las)
      ((atom? (car las)) (car las))
      (else (leftmost (car las))))))

(module+ test
  (check-equal? (leftmost '(((((1 2 3)) 4) 5 6) (7) 8)) 1))


;;;;;;;;;;;;;; From Chapter 4
(define eqan? ; <-------------------------------- SAME_ATOM_NUMBER?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
;;;;;;;;;;;;;; From Chapter 4

; Since number? and atom? dont differentiate between '2 and 2, I rather simplify and use eq? instead of equan?

(define eqlist?
  (lambda (las1 las2)
    (cond
      ((and (null? las1) (null? las2)) #t)                                            ; both null (end => true)
      ((or (null? las1) (null? las2))  #f)                                            ; one null, the other is not (false)
      ((and (atom? (car las1)) (atom? (car las2)))                                    ; both atoms
       (cond
         ((eq? (car las1) (car las2)) (eqlist? (cdr las1) (cdr las2)))                ;   the same => go on
         (else #f)))                                                                  ;   unequal (false)
      ((or (atom? (car las1)) (atom? (car las2))) #f)                                 ; one is atom, the other not (false)
      (else (and (eqlist? (car las1) (car las2)) (eqlist? (cdr las1) (cdr las2))))))) ; both lists => go on [car & cdr]

(module+ test
  (check-false (eqlist? '(banana ((split))) '((banana) split)))
  (check-true  (eqlist? '((banana) split) '((banana) split)))
  (check-true  (eqlist? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 ((5))))))
  (check-false (eqlist? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 (5))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or  (atom? s1) (atom? s2)) #f)
      (else (eqlist_2? s1 s2)))))

(define eqlist_2?
  (lambda (las1 las2)
    (cond
      ((and (null? las1) (null? las2)) #t)
      ((or (null? las1) (null? las2))  #f)
      (else
       (and (equal? (car las1) (car las2)) (eqlist_2? (cdr las1) (cdr las2)))))))
       ; this last piece of refactoring aint easy to understand

(module+ test
  (check-false (eqlist_2? '(banana ((split))) '((banana) split)))
  (check-true  (eqlist_2? '((banana) split) '((banana) split)))
  (check-true  (eqlist_2? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 ((5))))))
  (check-false (eqlist_2? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 (5))))))

; SIXTH COMMANDMENT
; Refactor (only) after tests pass

(define rember_mess
  (lambda (s las)
    (cond
      ((null? las) las)
      ((atom? (car las))
       (cond
         ((equal? s (car las)) (cdr las))
         (else (cond (car las) (rember_mess s (cdr las))))))
       (else
        (cond
          ((equal? s (car las)) (cdr las))
          (else (cons (car las) (rember_mess s (cdr las)))))))))

(define rember
  (lambda (s las)
    (cond
      ((null? las) las)
      ((equal? s (car las)) (cdr las))
      (else (cons (car las) (rember s (cdr las)))))))
