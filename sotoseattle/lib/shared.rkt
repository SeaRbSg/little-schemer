#lang racket
(require rackunit)

(provide atom?)

; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

; make sure atom? is defined #=> should output #f
(check-false (atom? (quote())))
