#lang racket
(require test-engine/racket-tests)

; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))
(check-expect (atom? (quote())) #f)


(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((atom? (car lat)) 
        (cond
          ((eq? a (car lat)) (rember* a (cdr lat)))
          (else (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat))))))) ; HOLY GUACAMOLE

(check-expect (rember* 1 '((1 2 3) 1)) '((2 3)))
(check-expect (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
(check-expect (rember* 'sauce '(((tomate sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomate)) ((bean)) (and ((flying)))))

(define lat?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((atom? (car lat)) (lat? (cdr lat)))
         (else #f))))))
   
(check-expect (lat? '((1 2 3) 1)) #f)
(check-expect (lat? '(1 2 3 1))   #t)
(check-expect (lat? '(1 2 ((3)) 1)) #f)

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((atom? (car lat))
       (cond
         ((eq? old (car lat)) (cons old (cons new (insertR new old (cdr lat)))))
         (else (cons (car lat) (insertR new old (cdr lat))))))
       (else (cons (insertR new old (car lat)) (insertR new old (cdr lat)))))))


(check-expect 
 (insertR 'roast 'chuck '(((how much (wood)) could ((a wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)) 
 '(((how much (wood)) could ((a wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))

(define occur*
  (lambda (a lat)
    (cond
      ((null? lat) 0)                                    ; question 1
      ((atom? (car lat))                                 ; question 2
       (cond
         ((eq? a (car lat)) (add1 (occur* a (cdr lat)))) ; counts and adds1 only in single place!!
         (else (occur* a (cdr lat)))))
      (else                                              ; question 3
       (+ (occur* a (car lat)) (occur* a (cdr lat))))))) ; recurring on car and cdr

(check-expect (occur* 1 '(1 2 3 (1 (2) ((3 4 1 (1 1)))))) 5)
(check-expect (occur* 1 '(1 1 1 1 2)) 4)

(define subst*
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((atom? (car lat))
       (cond
         ((eq? old (car lat)) (cons new (subst* new old (cdr lat))))
         (else (cons (car lat) (subst* new old (cdr lat))))))
      (else (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))

(check-expect (subst* 0 1 '(1 2 3 (1 (2) ((3 4 1 (1 1)))))) '(0 2 3 (0 (2) ((3 4 0 (0 0))))))

(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((atom? (car lat)) 
       (cond
         ((eq? old (car lat)) (cons new (cons old (insertL* new old (cdr lat))))) ; again, new+old insertion happens only here!!
         (else (cons (car lat) (insertL* new old (cdr lat))))))
      (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(check-expect 
 (insertL* 'pecker 'chuck '(((how much (wood)) could ((a wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
 '(((how much (wood)) could ((a wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))
              

(define member*
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat)) 
       (cond
         ((eq? a (car lat)) #t)
         (else (member* a (cdr lat)))))  ; can be simplified with (or !!
      (else (or (member* a (car lat)) (member* a (cdr lat)))))))

(check-expect (member* 'chips '((potato) (chips ((with) fish) (chips)))) #t)
(check-expect (member* 'chump '((potato) (chips ((with) fish) (chips)))) #f)

(define leftmost
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((atom? (car lat)) (car lat))
      (else (leftmost (car lat))))))

(check-expect (leftmost '(((((1 2 3)) 4) 5 6) (7) 8)) 1)

(define eqlist?
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) #t)                                            ; both null (end => true)
      ((or (null? lat1) (null? lat2))  #f)                                            ; one null, the other is not (false)
      ((and (atom? (car lat1)) (atom? (car lat2)))                                    ; both atoms
       (cond
         ((eq? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2)))                ;   the same => go on
         (else #f)))                                                                  ;   unequal (false)
      ((or (atom? (car lat1)) (atom? (car lat2))) #f)                                 ; one is atom, the other not (false)
      (else (and (eqlist? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2))))))) ; both lists => go on [car & cdr]

(check-expect (eqlist? '(banana ((split))) '((banana) split)) #f)
(check-expect (eqlist? '((banana) split) '((banana) split))   #t)
(check-expect (eqlist? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 ((5))))) #t)
(check-expect (eqlist? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 (5))))   #f)

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or  (atom? s1) (atom? s2)) #f)
      (else (eqlist_2? s1 s2)))))

(define eqlist_2?
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) #t)
      ((or (null? lat1) (null? lat2))  #f)
      (else
       (and (equal? (car lat1) (car lat2)) (eqlist_2? (cdr lat1) (cdr lat2))))))) ; this refactoring aint easy to understand

(check-expect (eqlist_2? '(banana ((split))) '((banana) split)) #f)
(check-expect (eqlist_2? '((banana) split) '((banana) split))   #t)
(check-expect (eqlist_2? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 ((5))))) #t)
(check-expect (eqlist_2? '((1 (2 3) 4 ((5)))) '((1 (2 3) 4 (5))))   #f)

; at this point in life I start sweating buckets

(define rember_mess
  (lambda (s l)
    (cond
      ((null? l) l)
      ((atom? (car l))
       (cond
         ((equal? s (car l)) (cdr l))
         (else (cond (car l) (rember_mess s (cdr l))))))
       (else 
        (cond
          ((equal? s (car l)) (cdr l))
          (else (cons (car l) (rember_mess s (cdr l)))))))))
               
(define rember
  (lambda (s l)
    (cond
      ((null? l) l)
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

; I still need to review it all because of my liberal use of eq? as equal?, eqan?, eqlist?, eqwtf?, etc.

(test)