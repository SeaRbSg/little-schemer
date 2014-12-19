; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

; Chapter 3

; first pass of rember
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember a
                            (cdr lat))))))))

(rember 'bacon '(bacon lettuce and tomato))
(rember 'and '(bacon lettuce and tomato))

; fixing rember
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a
                                  (cdr lat)))))))))

(rember 'and '(bacon lettuce and tomato))

; refactoring rember
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(rember 'and '(bacon lettuce and tomato))
(rember 'sauce '(soy sauce and tomato sauce))

; firsts
(define firsts
  (lambda (l)
    (cond
      ((null? l ) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(firsts '((a b) (c d) (e f)))

; insertR first pass
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cdr lat))
          (else (cons (car lat)
                      (insertR new old
                               (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for desert))

; insertR second pass
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
           (cons new (cdr lat )))
          (else (cons (car lat)
                      (insertR new old
                               (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for desert))

; insertR working version
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
           (cons old
                 (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR new old
                               (cdr lat)))))))))


(insertR 'topping 'fudge '(ice cream with fudge for desert))

; insertR refactored
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old
                           (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for desert))

; insertL
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old (cdr lat))))
      (else (cons (car lat)
                  (insertL new old
                           (cdr lat)))))))

(insertL 'fudge 'topping '(ice cream with topping for desert))

; insertL refactor
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new lat))
      (else (cons (car lat)
                  (insertL new old
                           (cdr lat)))))))

(insertL 'fudge 'topping '(ice cream with topping for desert))

; subst
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old
                         (cdr lat)))))))

(subst 'topping 'fudge '(ice cream with fudge for desert))

; subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) o1)
       (cons new (cdr lat)))
      ((eq? (car lat) o2)
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2
                          (cdr lat)))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

; subst2 refactored
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2
                          (cdr lat)))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

; multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup))

; multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old
                                (cdr lat)))))))

(multiinsertR 'vanilla 'chocolate '(chocolate ice cream sundae with chocolate sauce))

; multiinsertL first pass
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   ((multiinsertL new old
                                 lat)))))
      (else (cons (car lat)
                  (multiinsertL new old
                                (cdr lat)))))))

; does not complete
; (multiinsertL 'fried 'fish '(chips and fish or fish and fried))

; multiinsertL take 2
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old
                                (cdr lat)))))))

(multiinsertL 'fried 'fish '(chips and fish or fish and fried))

; multisubst
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (multisubst new old
                         (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old
                              (cdr lat)))))))

(multisubst 'fried 'fish '(chips and fish or fish and fried))
