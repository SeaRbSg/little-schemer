#lang racket
(require rackunit)

(let ()
  (define kons
    (lambda (kar kdr)
      (lambda (selector)
        (selector kar kdr))))

  (define kar
    (lambda (pair)
      (pair (lambda (a b) a))))

  (define kdr
    (lambda (pair)
      (pair (lambda (a b) b))))

  (let [(pair (kons 'a 'b))]
    (check-equal? (kar pair) 'a)
    (check-equal? (kdr pair) 'b)))

(let ()
  (define bons
    (lambda (kar)
      (let [(kdr '())]
        (lambda (selector)
          (selector (lambda (x) (set! kdr x))
                    kar
                    kdr)))))
  (define kar
    (lambda (c)
      (c (lambda (s a d) a))))
  
  (define kdr
    (lambda (c)
      (c (lambda (s a d) d))))
  
  (define set-kdr
    (lambda (c x)
      (let [(setter (c (lambda (s a d) s)))]
        (setter x))))
  
  (define kons
    (lambda (a d)
      (let [(pair (bons a))]
        (set-kdr pair d)
        pair)))
  
  (let [(pair (bons 'a))]
    (check-equal? (kar pair) 'a)
    (check-equal? (kdr pair) '()))
  
  (let [(pair (kons 'x 'y))]
    (check-equal? (kar pair) 'x)
    (check-equal? (kdr pair) 'y)))
