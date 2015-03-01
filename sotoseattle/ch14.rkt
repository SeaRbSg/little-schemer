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

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)]
      [(null? l2) #f]
      [(eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))]
      [else #f])))

(module+ test
  [check-true (eqlist? '(1 2 3) '(1 2 3))]
  [check-false (eqlist? '(1 2 3) '(1))]
  [check-false (eqlist? '(1 2) '(1 2 3))])

(define rember1*-v1
  (lambda (a l)
    (cond
      [(null? l) l]
      [(atom? (car l)) (cond
                         [(eq? a (car l)) (cdr l)]
                         [else (cons (car l) (rember1*-v1 a (cdr l)))])]
      [else (cond
              [(eqlist? (rember1*-v1 a (car l)) (car l)) (cons (car l) (rember1*-v1 a (cdr l)))]
              [else (cons (rember1*-v1 a (car l)) (cdr l))])])))

(module+ test
  [check-equal? (rember1*-v1 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
                                 '((Swedish rye) (French (mustard turkey)) salad)])

; letreccifying...

(define rember1*-v2
  (lambda (a l)
    (letrec
      ((R (lambda (l)
          (cond
            [(null? l) l]
            [(atom? (car l)) (cond
                               [(eq? a (car l)) (cdr l)]
                               [else (cons (car l) (R (cdr l)))])]
            [else (cond
                    [(eqlist? (R (car l)) (car l)) (cons (car l) (R (cdr l)))]
                    [else (cons (R (car l)) (cdr l))])]))))
      (R l))))

(module+ test
  [check-equal? (rember1*-v2 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                                   '((pasta) pasta (noodles meat sauce) meat tomatoes)])

(define rember1*-v3
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              [(null? l) l]
              [(atom? (car l)) (cond
                                 [(eq? a (car l)) (cdr l)]
                                 [else (cons (car l) (R (cdr l)))])]
              
              [else
                (let 
                  ((av (R (car l))))
                  (cond
                    [(eqlist? av (car l)) (cons (car l) (R (cdr l)))]
                    [else (cons av (cdr l))]))]))))
      (R l))))

(module+ test
  [check-equal? (rember1*-v3 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                                   '((pasta) pasta (noodles meat sauce) meat tomatoes)])

(define depth*-v1
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth*-v1 (cdr l))]
      [else (cond 
              [(> (depth*-v1 (cdr l)) (+ 1 (depth*-v1 (car l)))) (depth*-v1 (cdr l))]
              [else (+ 1 (depth*-v1 (car l)))])])))
    
    
(module+ test 
  [check-equal? (depth*-v1 '((pickled) peppers (peppers pickled))) 2]
  [check-equal? (depth*-v1 '(1 2 (3 4 (5 6 7 8)))) 3]
  [check-equal? (depth*-v1 '(hola)) 1])

(define depth*-v2
  (lambda (l)
    (cond
      [(null? l) 1]
      [else (let ((d_cdr (depth*-v2 (cdr l))))
              (cond
                [(atom? (car l)) d_cdr]
                [else (let ((d_car (depth*-v2 (car l))))
                        (cond 
                          [(> d_cdr (+ 1 d_car)) d_cdr]
                          [else (+ 1 d_car)]))]))])))
  
    
(module+ test 
  [check-equal? (depth*-v2 '((pickled) peppers (peppers pickled))) 2]
  [check-equal? (depth*-v2 '(1 2 (3 4 (5 6 7 8)))) 3]
  [check-equal? (depth*-v2 '(hola)) 1])

(define depth*-v3
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth*-v3 (cdr l))]
      [else (let ((d_cdr (depth*-v3 (cdr l)))
                  (d_car (+ 1 (depth*-v3 (car l)))))
              (cond 
                [(> d_cdr d_car) d_cdr]
                [else d_car]))])))
    
(module+ test 
  [check-equal? (depth*-v3 '((pickled) peppers (peppers pickled))) 2]
  [check-equal? (depth*-v3 '(1 2 (3 4 (5 6 7 8)))) 3]
  [check-equal? (depth*-v3 '(hola)) 1])

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*-v4
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth*-v4 (cdr l))]
      [else (let ((d_cdr (depth*-v4 (cdr l)))
                  (d_car (+ 1 (depth*-v4 (car l)))))
              (max d_cdr d_car))])))
    
(module+ test 
  [check-equal? (depth*-v4 '((pickled) peppers (peppers pickled))) 2]
  [check-equal? (depth*-v4 '(1 2 (3 4 (5 6 7 8)))) 3]
  [check-equal? (depth*-v4 '(hola)) 1])

(define depth*-v5
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth*-v5 (cdr l))]
      [else (max (depth*-v5 (cdr l)) (+ 1 (depth*-v5 (car l))))])))
    
(module+ test 
  [check-equal? (depth*-v5 '((pickled) peppers (peppers pickled))) 2]
  [check-equal? (depth*-v5 '(1 2 (3 4 (5 6 7 8)))) 3]
  [check-equal? (depth*-v5 '(hola)) 1])

; from ch12

(define scramble-v2
  (letrec
      ((X
        (letrec
            ((pick (lambda (n l)
                     (cond
                       [(eq? n '1) (car l)]
                       [else (pick (- n 1) (cdr l))]))))
          (lambda (wtf l)
            (cond
              ((null? l) l)
              (else (and
                     (cons (pick (car l) (cons (car l) wtf))
                           (X (cons (car l) wtf) (cdr l))))))))))
    (lambda (lat)
      (X '() lat))))

(module+ test
  (check-equal? (scramble-v2 '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (scramble-v2 '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (scramble-v2 '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

(define scramble-v3
  (letrec
      ((X
        (letrec
            ((pick (lambda (n l)
                     (cond
                       [(eq? n '1) (car l)]
                       [else (pick (- n 1) (cdr l))]))))
          (lambda (wtf l)
            (cond
              ((null? l) l)
              (else (let ((wip (cons (car l) wtf)))
                      (cons (pick (car l) wip)
                            (X wip (cdr l))))))))))
    (lambda (lat)
      (X '() lat))))

(module+ test
  (check-equal? (scramble-v3 '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
  (check-equal? (scramble-v3 '(1 2 3 4 5 6 7 8 9 10)) '(1 1 1 1 1 1 1 1 1 1))
  (check-equal? (scramble-v3 '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2)))

(define leftmost-ß1
  (lambda (l)
    (cond
      ((null? l) l)
      ((atom? (car l)) (car l))
      (else (let ((a (leftmost-ß1 (car l))))
              (cond
                ((atom? a) a)
                (else (leftmost-ß1 (cdr l)))))))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) l)
      ((atom? (car l)) (out (car l)))
      (else ;(let ()  <========================= this let is not necessary!!
              (lm (car l) out)
              (lm (cdr l) out)))));)


(define leftmost-ß2
  (lambda (l)
    (let/cc skip
      (lm l skip))))

(module+ tes
  (check-equal? (leftmost-ß2 '(((a)) b (c))) 'a))

(define leftmost-ß3
  (lambda (l)
    (letrec
      ((lm (lambda (l out)
             (cond
               ((null? l) l)
               ((atom? (car l)) (out (car l)))
               (else ;(let ()
                (lm (car l) out)
                (lm (cdr l) out))))));)
      (let/cc skip
        (lm l skip)))))

(module+ tes
  (check-equal? (leftmost-ß3 '(((a)) b (c))) 'a))

(define leftmost-ß4
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l out)
                 (cond
                   ((null? l) l)
                   ((atom? (car l)) (out (car l)))
                   (else ;(let ()
                    (lm (car l) out)
                    (lm (cdr l) out))))));)
        (lm l skip)
      ))))

(module+ tes
  (check-equal? (leftmost-ß4 '(((a)) b (c))) 'a))

(define leftmost-ß5
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) l)
                   ((atom? (car l)) (skip (car l)))
                   (else 
                    (lm (car l))
                    (lm (cdr l)))))))
        (lm l)
      ))))

(module+ tes
  (check-equal? (leftmost-ß5 '(((a 1 2)) b 3 (c 4))) 'a)
  (check-equal? (leftmost-ß5 '(() (() (a 1 2)) b 3 (c 4))) 'a))

(define rm
  (lambda (a l hop)
    (cond
      ((null? l) 'no)
      ((atom? (car l)) 
         (if (eq? a (car l)) 
             (cdr l) 
             (cons (car l) (rm (a (cdr l) hop)))))
      (else 
         (if (atom? (let/cc hop (rm a (car l) hop))) 
             (cons (car l) (rm a (cdr l) hop)) 
             (cons (rm a (car l) 0) (cdr l)))))))

