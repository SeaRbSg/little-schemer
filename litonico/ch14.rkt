(define (leftmost l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (car l)]
    [else (leftmost (car l))]))

(define (leftmost1 l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (car l)]
    [(atom? (leftmost (car l)))
     (leftmost (car l))]
    [else (leftmost (cdr l))]))

(define (leftmost2 l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (car l)]
    [else
      (let ((a (leftmost (car l))))
        (cond
          [(atom? a) a]
          [else (leftmost (cdr l))]))]))


(define (depth* l)
  (let ((a (add1 (depth* (car l))))
        (d (depth* (cdr l))))
    (cond
      [(null? l) 1]
      [(atom? (car l) d)]
      [(> d a) d]
      [else a])))


