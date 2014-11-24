; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

; Chapter 2
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))
(lat? '((and eggs)))

(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (null? '()))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(member? 'meat '(meat gravy))
(member? 'meat '(potatoes and meat gravy))
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '())
(member? 'liver '(lox))
(member? 'liver '(bagels and lox))
