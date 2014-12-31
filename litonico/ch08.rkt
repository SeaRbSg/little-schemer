#lang racket

(define rember-f ;; a.k.a deleteBy
  (lambda (test? a l)
    (cond
      [(null? l) '()]
      [(test? (car l) a) (cdr l)]
      [else (cons (car l)
                  (rember-f test? a (cdr l)))])))

(define eq?-c  
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f-curry
  (lambda (test?)
    (lambda (a l)
        (cond
          [(null? l) '()]
          [(test? (car l) a) (cdr l)]
          [else (cons (car l)
                      ((rember-f-curry test?) a (cdr l)))]))))

;; Slightly different from the Little Schemer's version-- 
;; theirs replaces the first occurance only
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
        (cond
          [(null? l) '()]
          [(test? (car l) old)
           (cons new 
                 (cons old 
                       ((insertL-f test?) new old (cdr l))))] 
          [else (cons (car l) 
                      ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
        (cond
          [(null? l) '()]
          [(test? (car l) old)
           (cons old 
                 (cons new 
                       ((insertR-f test?) new old (cdr l))))] 
          [else (cons (car l) 
                      ((insertL-f test?) new old (cdr l)))]))))

(define insert-g ;; why g?
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(eq? (car l) old)
         (seq new old (cdrl l))]
        [else (cons (car l) 
                    ((insert-g seq) new old (cdr l)))]))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-gL
  (insert-g seqL))

(define insert-gR
  (insert-g seqR))

(define insert-gL-anon
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define subst
  (lambda (new old l)
    (cond
      [(null? l) '()] 
      [(eq? (car l) old)
       (cons new (cdr l))]
      [else (cons (car l) 
                  (subst new old (cdr l)))])))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
      [(eq? x '+) +]
      [(eq? x '*) *]
      [else expt])))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-to-function (operator nexp))
             (value (fst-sub-exp nexp))
             (value (snd-sub-exp nexp)))])))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? a (car lat))
         ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat)
                    ((multirember-f test?) a (cdr lat)))]))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
    (lambda (test? lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat))
         (multiremberT test? (cdr lat))]
        [else (cons (car lat)
                    (multirember-f test? (cdr lat)))])))

;; And now the confusing part

;; multirember&co ::  a -> [a] -> (a -> b) -> b
;; Except `b` could also be a list, or any type
(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat) (col '() '())]
      [(eq? (car lat) a)
       (multirember&co a
                       (cdr lat) 
                       (lambda (newlat seen) 
                         (col newlat (cons (car lat) seen))))]
      [else (multirember&co a (cdr lat) 
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons (car lat) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen))) ;; man, this code-as-data thing

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertR new old (cdr lat)))])))

(define multiinsertR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertR new oldL oldR (cdr lat))))]
      [(eq? (car lat) old)
       (cons oldR
             (cons new
                   (multiinsertR new oldL oldR (cdr lat))))]
      [else (cons (car lat) 
                  (multiinsertR new oldL oldR (cdr lat)))])))
