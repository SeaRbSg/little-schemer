#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch02.rkt")
(require "ch03.rkt")

(provide build first second third a-pair? revpair union)

(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [else
        (cond
          [(member? (car lat) (cdr lat)) #f]
          [else (set? (cdr lat))])])))

(check-equal? (set? '(apple peaches apple plum)) #f)
(check-equal? (set? '(apple peaches plum)) #t)

(define set2?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(check-equal? (set2? '(apple peaches apple plum)) #f)
(check-equal? (set2? '(apple peaches plum)) #t)
(check-equal? (set2? '(apple 3 pear 4 9 apple 3 4)) #f)

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) (quote ())]
      [(member? (car lat) (cdr lat))
       (makeset (cdr lat))]
      [else
        (cons (car lat) (makeset (cdr lat)))])))

(check-equal? (makeset '(apple peach pear peach plum apple lemon peach)) '(pear plum apple lemon peach))

(define makeset2
  (lambda (lat)
    (cond
      [(null? lat) (quote ())]
      [else
        (cons (car lat)
              (makeset2 (multirember (car lat) (cdr lat))))])))


(check-equal? (makeset2 '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))
(check-equal? (makeset2 '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9))

(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else
        (cond
          [(member? (car set1) set2)
           (subset? (cdr set1) set2)]
          [else #f])])))

(check-equal? (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 onces horseradish)) #f)

(define subset2?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [(member? (car set1) set2)
       (subset2? (cdr set1) set2)]
      [else #f])))

(check-equal? (subset2? '(4 pounds of horseradish) '(four pounds chicken and 5 onces horseradish)) #f)

(define subset3?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2)
       (subset3? (cdr set1) set2))])))

(check-equal? (subset3? '(4 pounds of horseradish) '(four pounds chicken and 5 onces horseradish)) #f)

(define eqset?
  (lambda (set1 set2)
    (cond
      [(and (subset? set1 set2) (subset? set2 set1))]
      [else #f])))

(check-equal? (eqset? '(6 large chickens with wings) '(6 chickens with large wings)) #t)


(define eqset2?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(check-equal? (eqset2? '(6 large chickens with wings) '(6 chickens with large wings)) #t)


(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else
        (cond
          [(member? (car set1) set2) #t]
          [else
            (intersect? (cdr set1) set2)])])))

(check-equal? (intersect? '(stewed tomatoes and macaroni) '(macroni and chese)) #t)

(define intersect2?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [(member? (car set1) set2) #t]
      [else
        (intersect2? (cdr set1) set2)])))

(check-equal? (intersect2? '(stewed tomatoes and macaroni) '(macroni and chese)) #t)

(define intersect3?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [(or (member? (car set1) set2)
        (intersect3? (cdr set1) set2))])))

(check-equal? (intersect3? '(stewed tomatoes and macaroni) '(macroni and chese)) #t)

(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) (quote ())]
      [(member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(check-equal? (intersect '(stewed tomatoes and macaroni) '(macaroni and chesese)) '(and macaroni))

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) set2]
      [else (cons (car set1) (union (cdr set1) set2))])))

(check-equal? (union '(hot dogs and) '(ketchup pickles and mustard)) '(hot dogs ketchup pickles and mustard))
(check-equal? (union '(tea) '(and cake)) '(tea and cake))

(define xxx
  (lambda (set1 set2)
    (cond
      [(null? set1) (quote ())]
      [(member? (car set1) set2)
       (xxx (cdr set1) set2)]
      [else (cons (car set1) (xxx (cdr set1) set2))])))

(check-equal? (xxx '(breakfast brunch lunch dinner) '(brunch)) '(breakfast lunch dinner))

(define difference
  (lambda (set1 set2)
    (cond
      [(null? set1) (quote ())]
      [(member? (car set1) set2)
       (xxx (cdr set1) set2)]
      [else (cons (car set1) (difference (cdr set1) set2))])))

(check-equal? (difference '(breakfast brunch lunch dinner) '(brunch)) '(breakfast lunch dinner))

(define intersectall
  (lambda (lset)
    (cond
      [(null? (cdr lset)) (car lset)]
      [else (intersect (car lset) (intersectall (cdr lset)))])))

(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples))) '(6 and))

(check-equal? (pair? '(par pear)) #t)
(check-equal? (pair? '(3 7)) #t)
(check-equal? (pair? '((2) (pair))) #t)
(check-equal? (pair? '(full (house))) #t)

(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(check-equal? (a-pair? '(full (house))) #t)

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(check-equal? (build 'x 'y) '(x y))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(check-true (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))

(define revrel
  (lambda (rel)
    (cond
      [(null? rel) (quote ())]
      [else (cons (build
                    (second (car rel))
                    (first (car rel)))
                  (revrel (cdr rel)))])))

(check-equal? (revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(check-equal? (revpair '(a b)) '(b a))

(define revrel2
  (lambda (rel)
    (cond
      [(null? rel) (quote ())]
      [else (cons (revpair (car rel))
                  (revrel2 (cdr rel)))])))

(check-equal? (revrel2 '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

(define seconds
  (lambda (l)
    (cond
      [(null? l) (quote ())]
      [else (cons (second (car l))
                  (seconds (cdr l)))])))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(check-false (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-true (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(check-false (fullfun? '((grape raisin) (plum prune) (stewed prune))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))

(check-false (one-to-one? '((grape raisin) (plum prune) (stewed prune))))

