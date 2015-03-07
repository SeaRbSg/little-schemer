(set! x 'skins)

(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(set! x 'rings)

(gourmet 'onion)
;; (onion rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(define omnivore
  (let ((x 'minestrone)))
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(define gobbler
  (let ((x 'minestrone)))
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                    (cons x '()))))))

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))
