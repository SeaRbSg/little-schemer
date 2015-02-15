#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; From previous chapters
(define multirember 
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (multirember a (cdr lat))]
     [else (cons (car lat) (multirember a (cdr lat)))])))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

;; Ch7

(define set? 
  (lambda (lat)
    (cond 
     [(null? lat) #t]
     [(member (car lat) (cdr lat)) #f]
     [else (set? (cdr lat))])))

(test-case "set?"
           [check-false (set? '(apple peaches apple plum))]
           [check-true  (set? '(apples peaches pears plums))]
           [check-true  (set? '())]
           [check-false (set? '(apple 3 pear 4 9 apple 3 4))])


(define makeset
  (lambda (lat)
    (cond
     [(null? lat) lat]
     [else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))])))

(test-case "makeset"
           [check-equal? 
            (makeset '(apple peach pear peach plum apple lemon peach)) 
            '(apple peach pear plum lemon)]
           [check-equal?
            (makeset '(apple 3 pear 4 9 apple 3 4))
            '(apple 3 pear 4 9)])


;; (define subset?
;;   (lambda (set1 set2)
;;     (cond
;;      [(null? set1) #t]
;;      [(member (car set1) set2) (subset? (cdr set1) set2)]
;;      [else #f])))

(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [else (and (member (car set1) set2)
                (subset? (cdr set1) set2))])))

(test-case "subset?"
           [check-true (subset? '() '(apple beans))]
           [check-true  (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))]
           [check-false (subset? '(4 pounds of horseradish) '(four pounds of chicken and 5 ounces horseradish))])


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(test-case "eqset?"
           [check-true (eqset? '(6 large chickens with wings)
                               '(6 chickens with large wings))])

;; (define intersect?
;;   (lambda (set1 set2)
;;     (cond
;;      [(null? set1) #f]
;;      [(member (car set1) set2) #t]
;;      [else (intersect? (cdr set1) set2)])))

(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [else (or (member (car set1) set2)
               (intersect? (cdr set1) set2))])))

(test-case "intersect?"
           [check-not-false (intersect? '(a) '(a b c))]
           [check-not-false (intersect? '(stewed tomatoes and macaroni)
                                        '(macaroni and cheese))])
;; NOTE: had to use check-not-false here because intersect? returns the first
;; no false value

(define intersect
  (lambda (set1 set2)
    (cond
     [(null? set1) set1]
     [(member (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
     [else (intersect (cdr set1) set2)])))

(test-case "intersect"
           [check-equal? (intersect '(stewed tomatoes and macaroni)
                                    '(macaroni and cheese))
                         '(and macaroni)])


(define union
  (lambda (set1 set2)
    (cond
     [(null? set1) set2]
     [(member (car set1) set2) (union (cdr set1) set2)]
     [else (cons (car set1) (union (cdr set1) set2))])))

(test-case "union"
           [check-equal? (union '(stewed tomatoes and macaroni casserole)
                                '(macaroni and cheese))
                         '(stewed tomatoes casserole macaroni and cheese)])


;; This is my solution
;; (define intersectall
;;   (lambda (l-set)
;;     (cond
;;      [(null? l-set) l-set]
;;      [(null? (cdr l-set)) (car l-set)]
;;      [(null? (cddr l-set)) (intersect (car l-set) (cadr l-set))]
;;      [else (intersectall (cons (intersect (car l-set) (cadr l-set)) (cddr l-set)))])))

;; The book's much cleaner solution
(define intersectall
  (lambda (l-set)
    (cond
     [(null? l-set) l-set]
     [(null? (cdr l-set)) (car l-set)]
     [else (intersect (car l-set) (intersectall (cdr l-set)))])))

(test-case "intersectall"
           [check-equal? (intersectall '()) '()]
           [check-equal? (intersectall '((a))) '(a)]
           [check-equal? (intersectall '((a b) (a c))) '(a)]
           [check-equal? (intersectall '((a b) (a b c) (b c))) '(b)]
           [check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
                         '(a)]
           [check-equal? (intersectall '((6 pears and)
                                         (3 peaches and 6 peppers)
                                         (8 pears and 6 plums)
                                         (and 6 pruse with some apples)))
                         '(6 and)])


(define a-pair?
  (lambda (l)
    (cond 
     [(null? l) #f]
     [(atom? l) #f]
     [(null? (cdr l)) #f]
     [(null? (cddr l)) #t]
     [else #f])))

(test-case "a-pair?"
           [check-true (a-pair? '(pear pear))]
           [check-true (a-pair? '(3 7))]
           [check-true (a-pair? '((2) (pair)))]
           [check-true (a-pair? '(full (house)))])


;; Given
(define my-first
  (lambda (p)
    (car p)))

(define my-second
  (lambda (p)
    (cdr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; Back to my stuff
(define my-third
  (lambda (l)
    (caddr l)))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(test-case "fun?"
           [check-true  (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))]
           [check-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))])


(define revrel
  (lambda (rel)
    (cond
     [(null? rel) rel]
     [else (cons (build (second (car rel)) (first (car rel)))
                 (revrel (cdr rel)))])))

(test-case "revrel"
           [check-equal? (revrel '((8 a) (pumpkin pie) (got sick)))
                         '((a 8) (pie pumpkin) (sick got))])


;; Given
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


(define revrel2
  (lambda (rel)
    (cond
     [(null? rel) rel]
     [else (cons (revpair (car rel)) (revrel2 (cdr rel)))])))

(test-case "revrel2"
           [check-equal? (revrel2 '((8 a) (pumpkin pie) (got sick)))
                         '((a 8) (pie pumpkin) (sick got))])


(define seconds
  (lambda (l)
    (cond
     [(null? l) l]
     [else (cons (cadr (car l)) (seconds (cdr l)))])))

(test-case "seconds"
           [check-equal? (seconds '((a b) (c d) (e f)))
                         '(b d f)])

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(test-case "fullfun?"
           [check-false (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))]
           [check-true  (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))]
           [check-false (fullfun? '((grape raise) (plum prune) (stewed prune)))]
           [check-true (fullfun? '((grape raise) (plum prune) (stewed grape)))])


(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(test-case "one-to-one?"
           [check-false (one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4)))]
           [check-true  (one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4)))]
           [check-false (one-to-one? '((grape raise) (plum prune) (stewed prune)))]
           [check-true (one-to-one? '((grape raise) (plum prune) (stewed grape)))])


