#lang racket
(require "lib/shared.rkt")
(require rackunit)
;(require test-engine/racket-tests)
;(require racket/trace)

; we start by writing (leftmost lsex), which extracts the leftmost atom of a list of S-exp

(define leftmost-v1
  (lambda (l)
    (cond
      ;[(null? l) l]                   ; not needed assuming the list cannot be empty
      [(atom? (car l)) (car l)]
      [else (leftmost-v1 (car l))])))

(module+ test
  [check-equal? (leftmost-v1 '(((a) b) (c d))) 'a]
  [check-equal? (leftmost-v1 '(((a) ()) () (e))) 'a]
  [check-exn exn:fail? (lambda () (leftmost-v1 '(((() a) ()))))] ; testing it throws error
  [check-exn exn:fail? (lambda () (leftmost-v1 '()))])

; let's make it bulletproof so it can accept empty lists and lists where the first sexp is not an atom
; in which case we do not want to return an empty list, but instead go on and find the first atom available

(define leftmost-v2
  (lambda (l)
    (cond
      [(null? l) l]
      [(atom? (car l)) (car l)]
      [else 
       (cond
         [(atom? (leftmost-v2 (car l))) (leftmost-v2 (car l))]
         [else (leftmost-v2 (cdr l))])])))

(module+ test
  [check-equal? (leftmost-v2 '(((a) b) (c d))) 'a]
  [check-equal? (leftmost-v2 '(((a) ()) () (e))) 'a]
  [check-equal? (leftmost-v2 '(() a b)) 'a]
  [check-equal? (leftmost-v2 '(((() a) ()))) 'a]
  [check-equal? (leftmost-v2 '()) '()])

; as a first step lets use letrec to remove the offending (leftmost-v2 (car l))

(define leftmost-v3
  (lambda (l)
    (letrec
      ((X (lambda (lol) (leftmost-v2 (car lol)))))
      (cond
        [(null? l) l]
        [(atom? (car l)) (car l)]
        [else 
         (cond
           [(atom? (X l)) (X l)]
           [else (leftmost-v3 (cdr l))])]))))

(module+ test
  [check-equal? (leftmost-v3 '(((a) b) (c d))) 'a]
  [check-equal? (leftmost-v3 '(((a) ()) () (e))) 'a]
  [check-equal? (leftmost-v3 '(() a b)) 'a]
  [check-equal? (leftmost-v3 '(((() a) ()))) 'a]
  [check-equal? (leftmost-v3 '()) '()])

; it is still an ugly bastard. Let's use let [remmember it always has a definition and a value]

(define leftmost-v4
  (lambda (l)
    (cond
      [(null? l) l]
      [(atom? (car l)) (car l)]
      [else 
       (let                                     ; open scope
         ((a (leftmost-v4 (car l))))            ; <===== defintion like letrecc
         (cond                                  ; \
           [(atom? a) a]                        ;  | <== value like letrec
           [else (leftmost-v4 (cdr l))]))])))   ; /

(module+ test
  [check-equal? (leftmost-v4 '(((a) b) (c d))) 'a]
  [check-equal? (leftmost-v4 '(((a) ()) () (e))) 'a]
  [check-equal? (leftmost-v4 '(() a b)) 'a]
  [check-equal? (leftmost-v4 '(((() a) ()))) 'a]
  [check-equal? (leftmost-v4 '()) '()])

; playing with it, lets extract the let further up

(define leftmost-v5
  (lambda (l)
    (let 
      ((a (leftmost-v5 (car l))))
      (cond
        [(null? l) l]
        [(atom? (car l)) (car l)]
        [else 
          (cond
           [(atom? a) a]
           [else (leftmost-v5 (cdr l))])]))))

; now, why doesn't the following work?
; QUESTION ==> is the following right
; because unlike letrec when we define a we are actually evaluating the expression (leftmost-v5 (car...
; at that point, and in some cases (car l) is not a list and it blows!
; so unlike letrec, that defines and does not evaluate (leaves it to when it is called)
; let evaluates there and then!!





