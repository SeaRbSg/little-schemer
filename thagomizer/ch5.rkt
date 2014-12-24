#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))


;; Chapter Work

(define rember*
  (lambda (a l)
    (cond 
     [(null? l) '()]
     [(not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l)))]
     [(eq? (car l) a) (rember* a (cdr l))]
     [else (cons (car l) (rember* a (cdr l)))])))


(test-case "rember*"
           [check-equal? (rember* 'cup 
                                  '((coffee) cup ((tea) cup) (and (hick)) cup))
                         '((coffee) ((tea)) (and (hick)))]
           [check-equal? (rember* 'sauce 
                                  '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
                         '(((tomato)) ((bean)) (and ((flying))))])


(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l))) (cons (insertR* new old (car l))
                                  (insertR* new old (cdr l)))]
     [(eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l))))]
     [else (cons (car l) (insertR* new old (cdr l)))])))


(test-case "insertR* new old l"
           [check-equal? (insertR* 'a 'b '()) '()]
           [check-equal? (insertR* 'b 'a '(a c d)) '(a b c d)]
           [check-equal? (insertR* 
                          'roast 
                          'chuck
                          '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
                         '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)])


(define occur*
  (lambda (a l)
    (cond     
     [(null? l) 0]
     [(not (atom? (car l))) (+ (occur* a (car l)) (occur* a (cdr l)))]
     [(eq? a (car l)) (+ 1 (occur* a (cdr l)))]
     [else (occur* a (cdr l))])))

(test-case "occur*"
           [check-eq? (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
                      5])

(define subst* 
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l))) (cons (subst* new old (car l)) (subst* new old (cdr l)))]
     [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
     [else (cons (car l) (subst* new old (cdr l)))])))

(test-case "subst* new old l"
           [check-equal? (subst* 
                          'orange 
                          'banana 
                          '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
                         '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy))])


(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l)))]
     [(eq? old (car l)) (cons new (cons old (insertL* new old (cdr l))))]
     [else (cons (car l) (insertL* new old (cdr l)))])))

(test-case "insertL* new old"
           [check-equal? (insertL* 'a 'b '()) '()]
           [check-equal? (insertL* 'a 'b '(a b)) '(a a b)]
           [check-equal? (insertL* 'a 'b '(b c d)) '(a b c d)]
           [check-equal? (insertL* 'a 'b '((b) b c d)) '((a b) a b c d)]
           [check-equal? (insertL* 'a 'b '((b b)))'((a b a b))]
           [check-equal? (insertL* 'p 'ch '((h m (w)) co ((a (w) ch)) (((ch))) (if (a) ((w ch))) co ch w)) '((h m (w)) co ((a (w) p ch)) (((p ch))) (if (a) ((w p ch))) co p ch w)])


(define member* 
  (lambda (a l)
    (cond
     [(null? l) #f]
     [(list? (car l)) (or (member* a (car l)) (member* a (cdr l)))]
     [(eq? a (car l)) #t]
     [else (member* a (cdr l))])))

(test-case "member* a l"
           [check-true (member* 'chips '((potato) (chips ((with) fish) (chisp))))])


(define leftmost
  (lambda (l) 
    (cond
     [(null? l) '()]
     [(list? (car l)) (leftmost (car l))]
     [else (car l)])))

(test-case "leftmost l"
           [check-eq? (leftmost '()) '()]
           [check-eq? (leftmost '(a b c)) 'a]
           [check-eq? (leftmost '(((hot) (tuna (and))) cheese)) 'hot]
           [check-eq? (leftmost '(((() four)) 17 (seventeen))) '()]
           [check-eq? (leftmost '((potato (chips (with) fish) (chips)))) 'potato])




(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [(and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1)) (atom? (car l2))) #f]
     [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(test-case "eqlist?" 
           [check-false (eqlist? '(strawberry ice cream) '(strawberry cream ice))]
           [check-false (eqlist? '(banana ((split))) '((banana) (split)))]
           [check-false (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))]
           [check-true (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))]
           [check-true (eqlist? '(strawberry ice cream) '(strawberry ice cream))])


(define my_equal?
  (lambda (s1 s2) 
    (cond 
     [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))


(define eqlist2?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [else (and (my_equal? (car l1) (car l2))
                (my_equal? (cdr l1) (cdr l2)))])))

(test-case "eqlist2?" 
           [check-false (eqlist2? '(strawberry ice cream) '(strawberry cream ice))]
           [check-false (eqlist2? '(banana ((split))) '((banana) (split)))]
           [check-false (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))]
           [check-true (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))]
           [check-true (eqlist2? '(strawberry ice cream) '(strawberry ice cream))])


(define rember
  (lambda (s l)
    (cond 
     [(null? l) '()]
     [(equal? (car l) s) (cdr l)]
     [else (cons (car l) (rember s (cdr l)))])))

(test-case "rember"
           [check-equal? (rember 'a '(a b c)) '(b c)]
           [check-equal? (rember '(a b) '(z (a b) x ((y)))) '(z x ((y)))])
