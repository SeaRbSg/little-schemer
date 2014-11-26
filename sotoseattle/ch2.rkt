#lang racket
; ______________INITIAL DEFINITIONS______________
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))
; make sure atom? is defined #=> should output #f
(atom? (quote()))

(define lat?                               ; define names a function
  (lambda (l)                              ; lambda creates a function with arguments
    (cond                                  ; cond asks questions
      ((null? l) #t)                       ; see 1st commandment
      ((atom? (car l)) (lat? (cdr l)))     ; a list with 2 S-expressions !! a question + another question
    (else #f))))

(lat?'(Jack Sprat could eat no chicken fat))
(lat?'(bacon and eggs))
(lat?'(bacon (and eggs)))

(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (atom? '(atom)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

; see questions in (cond as lists with other S-expr to evaluate in order
;  (cond
;    (question_1 something)
;       if question_1 is true go on and evaluate 'something' AND return it at its end
;       if question_1 is false, we dont get to evaluate something, instead we go to the next question
;       if we run out of questions (all false) we hit the else
; (else #whatever
;
; ((null? lat) #f) => evaluate if lat is null, if so evaluate #f (kind of return it) and don't do the 'else'

; FIRST COMMANDMENT: Always, always, always ask null? first in a function

(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '(bagels and lox))
