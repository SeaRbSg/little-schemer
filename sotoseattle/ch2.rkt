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
      ((atom? (car l)) (lat? (cdr l)))     ; 2 questions like &&
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
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; see questions in (cond as lists with other S-expr to evaluate in order, getting out if a question is false (?)
;  (cond
;    (question_1 something) if question_1 is true go on and evaluate something, then go on to question_2
;    (question_2 something) if question_1 is false get back up so you go on to else
; (else...
; ((null? lat) #f) => evaluate if lat is null, if true evaluate #f and do not do the else

; FIRST COMMANDMENT: Always, always, always ask null? first in a function

(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '(bagels and lox))
