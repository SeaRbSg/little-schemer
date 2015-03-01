(define intersect (set1 set2)
  (letrec
    (( I (lambda (set)
           (cond
             [(null? set) '()]
             [(member? (car set) set2)
              (cons (car set)
                    (I (cdr set)))]
             [else (I (cdr set))])))
     (I set1))))

(define intersectall
  (lambda (lset)
    (cond
      [(null? lset) '()]
      [(null? (cdr lset)) (car lset)]
      [else (intersect (car lset)
                       (intersectall
                         (cdr lset)))])))

(define intersectall/letcc
  (lambda (lset)
    (let/cc hop
      (letrec
        ((A (lambda (lset)
              (cond
                [(null? (car lset))
                 (hop '())]
                [(null? (cdr lset))
                 (car lset)]
                [else
                  (intersect (car lset)
                             (A (cdr lset)))]))))
        (cond
          [(null? lset) '()]
          [else (A lset)])))))


;; Inline intersect, so it can use the continuation on '().
;; This is a monstrosity.
(define intersectall/letcc
  (lambda (lset)
    (let/cc hop
      (letrec
        ((A (lambda (lset)
              (cond
                [(null? (car lset))
                 (hop '())]
                [(null? (cdr lset))
                 (car lset)]
                [else
                  (intersect (car lset)
                             (A (cdr lset)))]))))
        (I (lambda (set1 set2)
          (letrec
            (( J (lambda (set)
                   (cond
                      [(null? set) '()]
                      [(member? (car set) set2)
                       (J (cdr set))]
                      [else (cons (car s1)
                                  (J (cdr set)))]))))
        (cond
          [(null? lset) '()]
          [else (A lset)])))))
    (cond
        [(null? lset) '()]
        [else (A lset)]))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? (car lat) a) '()]
              [else (cons (car lat)
                          (R (cdr lat)))]))))
        (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
            (letrec
              ((R (lambda (lat)
                    (cond
                      [(null? lat) '()]
                      [(eq? (car lat) a)
                       (skip (R (cdr lat)))h]
                      [else
                        (cons (car lat)
                              (R (cdr lat)))]))))
              (R lat)))))
