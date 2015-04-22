(define sub1
  (lambda (m)
    (- m 1)))

(define deep
  (lambda (m)
    (cond
      [(zero? m) 'pizza]
      [else (cons (deep (sub1 m))
                  '())])))


(define toppings) ; :: Continuation

(define deepB
  (lambda (m)
    (cond
      [(zero? m)
       (let/cc jump
               (set! toppings jump)
               'pizza)]
      [else (cons (deepB (sub1 m))
                  '())])))

(define deep&coB
  (lambda (m k)
    (cond
      [(zero? m)
       (let () ; why do we need a `let` here?
         (set! toppings k)
         (k 'pizza))]
      [else
        (deep&coB (sub1 m)
                  (lambda (x)
                    (k (cons x '()))))])))

(define leave)

(define walk
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (leave (car l))]
      [else
        (let ()
          (walk (car l))
          (walk (cdr l)))])))

(define start-it ;; bluh
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))


(define waddle
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (let ()
         (let/cc rest
           (set! fill rest)
           (leave (car l)))
         (waddle (cdr l)))]
      [else
        (let ()
          (waddle (car l))
          (waddle (cdr l)))])))


