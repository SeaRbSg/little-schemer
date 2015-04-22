#lang typed/racket

(: box Number (-> Number (-> Number Nothing) Nothing))
(define box
  (lambda ([it : Number])
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

(: unbox (-> -> Number) Number)
(define unbox
  (lambda (box)
    (box (lambda ([it : Number] set) it))))
