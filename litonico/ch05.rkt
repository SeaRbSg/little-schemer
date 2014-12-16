#lang racket

(require "prelude.rkt")
(require rackunit)

(define rember* ;; Recursive rember
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) 
          (rember* a (cdr l))]
         [else (cons (car l) 
                     (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

(test-case "rember*"
           (check-equal? (rember* 'cup 
                                  '((coffee) cup ((tea) cup) (and (hick)) cup))
                         '((coffee) ((tea)) (and (hick))))
           (check-equal? (rember* 'sauce 
                                  '(((tomato sauce)) 
                                    ((bean) sauce) 
                                    (and ((flying)) sauce)))
                         '(((tomato)) ((bean)) (and ((flying))))))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) 
          (cons old 
                (cons new 
                      (insertR* new old (cdr l))))]
         [else (cons (car l) 
                     (insertR* new old (cdr l)))])]
      [else (cons 
              (insertR* new old (car l)) 
              (insertR* new old (cdr l)))])))

(test-case "insertR*"
           (check-equal? (insertR* 'roast 'chuck
                          '((how much (wood)) 
                            could ((a (wood) chuck)) 
                            (((chuck))) (if (a) ((wood chuck))) 
                            could chuck wood))
                         '((how much (wood))
                           could ((a (wood) chuck roast)) 
                           (((chuck roast))) 
                           (if (a) ((wood chuck roast))) 
                           could chuck roast wood)))

(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? (car l) a)
          (+ 1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))])]
       [else (+ (occur* a (car l))
                (occur* a (cdr l)))])))

(test-case "occur*"
           (check-eq? 
             (occur* 'banana 
                     '((banana) 
                       (split ((((banana ice))) (cream (banana)) sherbet)) 
                       (banana) (bread) (banana brandy)))
                      5))

(define subst* 
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons new 
                (subst* new old (cdr l)))]
         [else (cons (car l)
                     (subst* new old (cdr l)))])]
       [else (cons 
               (subst* new old (car l))
               (subst* new old (cdr l)))])))

(test-case "subst*"
           (check-equal? (subst* 'orange 'banana 
                          '((banana) 
                            (split ((((banana ice))) (cream (banana)) sherbet)) 
                            (banana) (bread) (banana brandy)))
                         '((orange) 
                           (split ((((orange ice))) (cream (orange)) sherbet)) 
                           (orange) (bread) (orange brandy))))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) 
          (cons new 
                (cons old 
                      (insertR* new old (cdr l))))]
         [else (cons (car l) 
                     (insertR* new old (cdr l)))])]
      [else (cons 
              (insertR* new old (car l)) 
              (insertR* new old (cdr l)))])))

(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
         (or (eq? (car l) a)
             (member* (cdr l) a))]
       [else (or (member* a (car l))
                 (member* a (cdr l)))])))

(test-case "member*"
           (check-true (member* 'chips 
                                '((potato) (chips ((with) fish) (chisp))))))

(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))


(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(and (null? l1) (atom? (car l2)) #f)]
      [(null? l1) #f]
      [(and (atom? (car l1)) (null? l2)#f)]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) 
            (eqlist? (cdr l1) (cdr l2)))]
      [(atom? (car l1) #f)]
      [(null? l2 #f)]
      [(atom? (car l2)) #f]
      [else
        (and (eqlist? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))])))


(define eqlist1?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) 
            (eqlist1? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2)))]
      [else
        (and (eqlist1? (car l1) (car l2))
             (eqlist1? (cdr l1) (cdr l2)))])))

(define my-equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)]
      [(atom? s1) #f]
      [(atom? s2) #f]
      [else (eqlist? s1 s2)])))

(define my-equal2?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) 
            (eqlist2? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2)))]
      [else
        (and (eqlist2? (car l1) (car l2))
             (eqlist2?(cdr l1) (cdr l2)))])))

(define rember
  (lambda (s l)
    (cond
      [(null? l) '()]
      [else (cond
              [(equal? (car l) s) (cdr l)]
              [else (cons (car l)
                          (rember s (cdr l)))])])))
