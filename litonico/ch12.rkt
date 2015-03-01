(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? a (car lat))
               (mr (cdr lat))]
              [else (cons (car lat)
                          (mr (cdr lat)))]))))
     lat)))

(define multirember1
  (lambda (a lat)
    ((letrec
       ((mr) (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? a (car lat))
               (mr (cdr lat))]
              [else (cons (car lat)
                          (mr (cdr lat)))])))
       mr)
     lat)))

(define (id a)
  (a))


(define multirember2
  (lambda (a lat)
    (letrec
      ((mr) (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? a (car lat))
               (mr (cdr lat))]
              [else (cons (car lat)
                          (mr (cdr lat)))])))
      (mr lat))))


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l)
                    ((rember-f test?) a (cdr l)))]))))

(define rember-eq? (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (letrec
      ((m-f
         (lambda (a lat)
           (cond
             [(null? lat) '()]
             [(test? (car lat) a)
              ((m-f  a (cdr lat)))]
             [else (cons (car lat)
                        (m-f a (cdr lat)))]))))
      m-f)))

;; skipping `member` in all of its variations...

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else (cons (car set1)
                  (union (cdr set1) set2))])))

(define union2
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
        (cond
          [(null? set) set2]
          [(member? (car set) set2)
           (U (cdr set) set2)]
          [else (cons (car set)
                      (U (cdr set) set2))]))))
       (U set1))))

(define two-in-a-row?
  (letrec
    ((W (lambda (a lat)
          (cond
            [(null? lat) #f]
            [else (or (eq? (car lat) a)
                      (W (car lat)
                         (cdr lat)))]))))))


(define fixpt
  (lambda (f x)
    (fixpt f (f x))))

(define y
  (lambda (f)
    (f (y f))))
