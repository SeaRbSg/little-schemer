#lang racket

(require "lib/shared.rkt")
(require rackunit)

(define member? ; <--------------------- MEMBER? from chp2
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(module+ test
  (check-false (set? '(1 2 1 3)))
  (check-true (set? '(1 2 3))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(module+ test
  (check-true (set? (makeset '(1 2 1 3 1 3 3))))
  (check-true (set? (makeset '(1 2 3)))))

(define multirember ; <--------------------- MULTIREMBER from chp3
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      (else
       (cond
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))

(define makeset_v2
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else (cons (car lat) (multirember (car lat) (makeset_v2 (cdr lat)))))))) ; my version
;     (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat)))))))) ; book version

;;;;;;;;;;;;; QUESTION 1: I have seen that many times the above happens. There is more than one way to
;;;;;;;;;;;;; to structure the recursive logic. Test pass for both cases. Am I missing something?
;;;;;;;;;;;;; a first difference is that, when using cons, the logic structure determines the order
;;;;;;;;;;;;; of stuff showing up in the list (beware)
;;;;;;;;;;;;; UPDATE: As far as I have seen this symetry is a mirage, sometimes happens sometimes it doesnt
;;;;;;;;;;;;; a great example of how we cannot take this symetry for granted: 

(define countdown
  (lambda (n)
    (cond
      ((zero? n) '())
      (else (cons n (countdown (sub1 n)))))))

(define countup
  (lambda (n acc)
    (cond
      ((zero? n) acc)
      (else (countup (sub1 n) (cons n acc))))))

(module+ test
  (check-equal? (countdown 3)   '(3 2 1))
  (check-equal? (countup 3 '()) '(1 2 3)))

;;;;;;;;;;;;;
;;;;;;;;;;;;;

(module+ test
  (check-true (set? (makeset_v2 '(1 2 1 3 1 3 3))))
  (check-true (set? (makeset_v2 '(1 2 3))))
  (check-equal? (makeset_v2 '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))
  (check-equal? (makeset_v2 '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9)))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(module+ test
  (check-true (subset? '(5 chicken wings) '(5 hamburgers 2 piece fried chicken and light duckling wings)))
  (check-false (subset? '(4 pounds horseradish) '(four pounds chicken and 5 ounces horseradish))))

(define subset_v2? ; instead of return the answer to a question, take advantage of it being a question and use (and
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset_v2? (cdr set1) set2))))))

(module+ test
  (check-true (subset_v2? '(5 chicken wings) '(5 hamburgers 2 piece fried chicken and light duckling wings)))
  (check-false (subset_v2? '(4 pounds horseradish) '(four pounds chicken and 5 ounces horseradish))))

(define eqset_soto? ; my aproach before looking the solution (there was no need to do all that)
  (lambda (set1 set2)
    (cond
      ((and (null? set1) (null? set2)) #t)
      ((member? (car set1) set2) (eqset_soto? (cdr set1) (multirember (car set1) set2)))
      (else #f))))

(module+ test
  (check-true (eqset_soto? '(6 large chicken with wings) '(6 large chicken with wings))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1)))) ; niiiiiiceeeeee

(module+ test
  (check-true (eqset? '(6 large chicken with wings) '(6 large chicken with wings))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

(module+ test
  (check-true  (intersect? '(1 2 3 4 5 6) '(6 7 8 9)))
  (check-false (intersect? '(1 2 3 4 5) '(6 7 8 9)))
  (check-false (intersect? '(1 2 3 4 5 6) '()))
  (check-false (intersect? '() '())))

(define intersect_v2? ; intersect with 'or'
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or  (member? (car set1) set2) (intersect_v2? (cdr set1) set2))))))
;     (else (and (member? (car set1) set2) (subset_v2?    (cdr set1) set2)))))) ; comparison with (subset?

(module+ test
  (check-true  (intersect_v2? '(1 2 3 4 5 6) '(6 7 8 9)))
  (check-false (intersect_v2? '(1 2 3 4 5) '(6 7 8 9)))
  (check-false (intersect_v2? '(1 2 3 4 5 6) '()))
  (check-false (intersect_v2? '() '())))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) set1)
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(module+ test
  (check-equal? (intersect '(1 2 3 4 5 6) '(6 7 8 9)) '(6)))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
;     (else (union (cdr set1) (cons (car set1) set2)))))) ; my initial approach (I blame the dislexya)
                                                          ; the logic determines the order of the atoms in the list

(module+ test
  (check-equal? (union '(1 2 3 4 5 6) '(6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (union '(6 7 8 9) '(1 2 3 4 5 6)) '(7 8 9 1 2 3 4 5 6)))

(define union_soto ; my version: just cons both sets and then call (makeset !!
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      (else (makeset (cons (car set1) (union_soto (cdr set1) set2)))))))

(module+ test
  (check-equal? (union_soto '(1 2 3 4 5 6) '(6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (union_soto '(6 7 8 9) '(1 2 3 4 5 6)) '(7 8 9 1 2 3 4 5 6)))

(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) set1)
      ((member? (car set1) set2) (xxx (cdr set1) set2))
      (else (cons (car set1) (xxx (cdr set1) set2))))))

(module+ test
  (check-equal? (xxx '(1 2 7 9 0) '(6 7 8 9)) '(1 2 0)))

(define intersectall
  (lambda (lst)
    (cond
      ((null? (cdr lst)) (car lst))
      (else (intersect (car lst) (intersectall (cdr lst)))))))

(module+ test
  (check-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a)))

(define a-pair?
  (lambda (x)
    (cond
      ((or (atom? x) (or (null? x) (null? (cdr x)))) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(module+ test
  (check-true (a-pair? '(1 2)))
  (check-true (a-pair? '((1) pepe)))
  (check-true (a-pair? '(juan (casimiro)))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '())))) ; remember the linked list nature of consing, that is why we need the '()

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

;;;;; entering a BS ZONE (zone of belief suspension)
;;;;; the function firsts is not defined anywhere, so I am going ahead and doing it myself

(define firsts
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons (first (car rel)) (firsts (cdr rel)))))))

(module+ test
  (check-equal? (firsts '((1 2) (3 4) (5 6))) '(1 3 5)))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel_v1
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons (build (second (car rel)) (first (car rel))) (revrel_v1 (cdr rel)))))))

(module+ test
  (check-equal? (revrel_v1 '((1 2) (3 4))) '((2 1) (4 3))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(module+ test
  (check-equal? (revrel '((1 2) (3 4))) '((2 1) (4 3))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons (second (car rel)) (seconds (cdr rel)))))))

(define fullfun?                                  ; assumes it is already fun
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?                               ; also assumes it is already fun
  (lambda (fun)
    (fun? (revrel fun))))

(define 100%fun?                                  ; without assumtion, takes in a rel, yet goes both ways: 
  (lambda (rel)                                   ; set of firsts and set of seconds,
    (and (fun? rel) (fun? (revrel rel)))))        ; which means is a set in three ways, so set for life.

;;;;;;;;;;;;; QUESTION 2: OMG Lito is right. A fun is a hash !!!
;;;;;;;;;;;;; This surely deserves some talking next Tuesday
;;;;;;;;;;;;; Here is my atempt at making fun

(define update_fun
  (lambda (key fun)
    (cond
      ((null? fun) fun)
      ((eq? key (first (car fun))) (cons (build key (add1 (second (car fun)))) (update_fun key (cdr fun))))
      (else (cons (car fun) (update_fun key (cdr fun)))))))

(define wordcount
  (lambda (lat fun)
    (cond
      ((null? lat) (reverse fun))                                                         ; for readability
      ((member? (car lat) (firsts fun)) (wordcount (cdr lat) (update_fun (car lat) fun))) ; if car is key in fun add 1
      (else (wordcount (cdr lat) (cons (build (car lat) 1) fun))))))                      ; else make new pair with  1

(module+ test
  (check-equal? (update_fun 'b '((a 3) (b 4) (c 5))) '((a 3) (b 5) (c 5)))
  (check-equal? (wordcount '(a b c d e b a d e f g h a c f b b) '()) '((a 3) (b 4) (c 2) (d 2) (e 2) (f 2) (g 1) (h 1)))
  (check-equal? (wordcount '(whatever lola wants lola gets) '()) '((whatever 1) (lola 2) (wants 1) (gets 1))))