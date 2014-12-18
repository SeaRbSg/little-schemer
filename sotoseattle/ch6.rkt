#lang racket

(require "lib/shared.rkt")
(require rackunit)

; Definition
; Arithmetic Expression: An atom (incl numbers) or two arithm_expr combined with + x ^

(define numbered_raw?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp)) ; if it is atom, it return the result of is_this_atom_a_number? (not the list!)
      (else
       (cond
        ((eq? (car (cdr aexp)) '+) (and (numbered_raw? (car aexp)) (numbered_raw? (car (cdr (cdr aexp))))))
        ((eq? (car (cdr aexp)) 'x) (and (numbered_raw? (car aexp)) (numbered_raw? (car (cdr (cdr aexp))))))
        ((eq? (car (cdr aexp)) '^) (and (numbered_raw? (car aexp)) (numbered_raw? (car (cdr (cdr aexp)))))))))))

(module+ test
  (check-false (numbered_raw? '(pepe + juan)))
  (check-true  (numbered_raw? '(1 + 2)))
  (check-true  (numbered_raw? '(1 + (2 + 1))))
  (check-true  (numbered_raw? '((1 x 2) ^ (2 + (1 + 1))))))

; If we assume that the aexp will always be well formed
; we can ignore the + x ^, and only look at the numbers themselves

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(module+ test
  (check-false (numbered? '(pepe + juan)))
  (check-true  (numbered? '(1 + 2)))
  (check-true  (numbered? '(1 + (2 + 1))))
  (check-true  (numbered? '((1 x 2) ^ (2 + (1 + 1))))))

;;;;;;;;;;;; FROM CHAPTER 4
(define o+ ; <-------------------------------- ADD
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define x ; <-------------------------------- MUTIPLY
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define ^ ; <-------------------------------- EXPONENTIATION
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      (else
       (cond
         ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
         ((eq? (car (cdr nexp)) 'x) (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
         ((eq? (car (cdr nexp)) '^) (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))))

;;;;;;;;;;;;; QUESTION 1
;;;;;;;;;;;;; Why ((atom? nexp)... why not ((number? nexp)...
;;;;;;;;;;;;; we are talking about number expressions && numbers are a subset of atoms
; because you would assume that only numbers are passed, and for anything else it would fail !!

;;;;;;;;;;;;; QUESTION 2
;;;;;;;;;;;;; could we somehow refactor all those +x^ into a single line?
;;;;;;;;;;;;; the idea would be to extract the operator
; yes, but it is not in the book. a bit more complex than it looks

(module+ test
  (check-equal? (value '(2 + 3)) 5)
  (check-equal? (value '(2 x 3)) 6)
  (check-equal? (value '(2 ^ 3)) 8)
  (check-equal? (value '(2 + (1 + 2))) 5))

; SEVENTH COMMANDMENT
; Recur on the subparts of the same nature
;   - on sublists of lists
;   - on subexpr of expressions

(define value_my_prn
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       (cond
         ((eq? (car nexp) '+) (o+ (value_my_prn (car (cdr nexp))) (value_my_prn (car (cdr (cdr nexp))))))
         ((eq? (car nexp) 'x) (x (value_my_prn (car (cdr nexp))) (value_my_prn (car (cdr (cdr nexp))))))
         ((eq? (car nexp) '^) (^ (value_my_prn (car (cdr nexp))) (value_my_prn (car (cdr (cdr nexp)))))))))))

(module+ test
  (check-equal? (value_my_prn '(+ 2 3)) 5)
  (check-equal? (value_my_prn '(x 2 3)) 6)
  (check-equal? (value_my_prn '(^ 2 3)) 8)
  (check-equal? (value_my_prn '(+ 2 (+ 1 (+ 1 1)))) 5))

; We can clarify and simplify by extracting sub methods

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define value_2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       (cond
         ((eq? (operator nexp) '+) (o+ (value_2 (1st-sub-exp nexp)) (value_2 (2nd-sub-exp nexp))))
         ((eq? (operator nexp) 'x) (x (value_2 (1st-sub-exp nexp)) (value_2 (2nd-sub-exp nexp))))
         ((eq? (operator nexp) '^) (^ (value_2 (1st-sub-exp nexp)) (value_2 (2nd-sub-exp nexp)))))))))

(module+ test
  (check-equal? (value_2 '(+ 2 3)) 5)
  (check-equal? (value_2 '(x 2 3)) 6)
  (check-equal? (value_2 '(^ 2 3)) 8)
  (check-equal? (value_2 '(+ 2 (+ 1 2))) 5))

; EIGHTH COMMANDMENT
; after tests pass, and simplifying, apply DRY by extracting helper methods
; similar to 4 pr of simple design: 1) test pass 2) expressive/simplify 3) DRY 4) minimize

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

;;;;;;;;;;;;; QUESTION 3
;;;;;;;;;;;;; aint the same (cons '() n) than (cons n '())
;;;;;;;;;;;;; (cons '2 '())                     => '(2)
;;;;;;;;;;;;; (cons '(pepe jaime) '2)           => '((pepe jaime) . 2) <== improper list ==> messy stuff
;;;;;;;;;;;;; (cons '() 2)                      => '(() . 2) <============ dotted list ====> messy stuff
; so cons  does NOT insert an atom/list into another
; think about it in terms of linked lists, you are appending the first one to the other linked list
; (cons 'a '(b)) == cons 'a into b-->null [A linked list] == a --> b --> null !!!
; you need to append to a list or an empty list (null itself) so it is proper !!!
; when you cons 2 2 => a.b == a|b ==> an improper list because it is not ending in null
; this is also natural because we tend to cons into something that comes from recursion (lat/las)

(define zub1
  (lambda (n)
    (cond
      ((sero? n) n) ; I rather check
      (else (cdr n)))))

(module+ test
  (check-true  (sero? '()))
  (check-false (sero? '(())))
  (check-equal? (edd1 '()) '(()))
  (check-equal? (edd1 '(() ())) '(() () ()))
  (check-equal? (zub1 '(() ())) '(()))
  (check-equal? (zub1 '()) '())
  )

(define w+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (w+ (edd1 n) (zub1 m))))))  ; my version
      ;(else (edd1 (w+ n (zub1 m))))))) ; the book's version

(module+ test
  (check-equal? (w+ '(() ()) '(())) '(() () ()))
  (check-equal? (w+ '() '(())) '(()))
  (check-equal? (w+ '(()) '(())) '(() ()))
  (check-equal? (w+ '() '()) '())
  )

(define lat?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((atom? (car lat)) (lat? (cdr lat)))
      (else #f)))) ; dont get the point of the shadows joke

(module+ test
  (check-false (lat? '(() () ()))))
