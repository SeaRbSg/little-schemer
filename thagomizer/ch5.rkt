#lang racket
(require rackunit)

;; Givens
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


(define rember*
  (lambda (a l)
    (cond 
     ((null? l) '())
     ((not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l))))
     ((eq? (car l) a) (rember* a (cdr l)))
     (else (cons (car l) (rember* a (cdr l)))))))


(test-case "rember*"
           (check-equal? (rember* 'cup 
                                  '((coffee) cup ((tea) cup) (and (hick)) cup))
                         '((coffee) ((tea)) (and (hick))))
           (check-equal? (rember* 'sauce 
                                  '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
                         '(((tomato)) ((bean)) (and ((flying))))))


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((not (atom? (car l))) (cons (insertR* new old (car l))
                                  (insertR* new old (cdr l))))
     ((eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l)))))
     (else (cons (car l) (insertR* new old (cdr l)))))))


(test-case "insertR* new old l"
           (check-equal? (insertR* 'a 'b '()) '())
           (check-equal? (insertR* 'b 'a '(a c d)) '(a b c d))
           (check-equal? (insertR* 
                          'roast 
                          'chuck
                          '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
                         '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)))


(define occur*
  (lambda (a l)
    (cond     
     ((null? l) 0)
     ((not (atom? (car l))) (+ (occur* a (car l)) (occur* a (cdr l))))
     ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
     (else (occur* a (cdr l))))))

(test-case "occur*"
           (check-eq? (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
                      5))

(define subst* 
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((not (atom? (car l))) (cons (subst* new old (car l)) (subst* new old (cdr l))))
     ((eq? old (car l)) (cons new (subst* new old (cdr l))))
     (else (cons (car l) (subst* new old (cdr l)))))))

(test-case "subst* new old l"
           (check-equal? (subst* 
                          'orange 
                          'banana 
                          '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
                         '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy))))


(test-case "insertL* new old ")
