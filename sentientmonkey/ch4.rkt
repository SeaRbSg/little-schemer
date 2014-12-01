; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x))  (not(null? x)))))

(atom? 14)
(number? -3)
(number? 3.14159)

(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
(sub1 0)

(zero? 0)
(zero? 1492)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(o- 14 3)
(o- 17 9)
(o- 18 25)

(tup? (2 11 3 79 47 6))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)) )))))

(addtup '(1 2 3 4 5))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(x 5 3)
(x 13 4)
(x 12 3)

; first pass of tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote ()))
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6))

; fail
; (tup+ '(3 7 8 1) '(4 6))


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
       (quote ()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
              (tup+
                (cdr tup1) (cdr tup2)))))))

(tup+ '(3 7 8 1) '(4 6))

; tup+ simplified
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
              (tup+
                (cdr tup1) (cdr tup2)))))))

(tup+ '(3 7 8 1) '(4 6))

; > implementation
(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (gt (sub1 n) (sub1 m))))))

(gt 12 133)
(gt 120 11)

; works (got it right the first time)
(gt 3 3)

; < definition
(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (lt (sub1 n) (sub1 m))))))

(lt 4 6)
(lt 8 3)
(lt 6 6)

; = definition
(define eq
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (eq (sub1 n) (sub1 m))))))

(eq 6 6)

; = rewrite
(define eq
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
      (else #t))))

(eq 6 6)

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
        (x n (pow n (sub1 m)))))))

(pow 1 1)
(pow 2 3)
(pow 5 3)


; mystery function
(define ???
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (??? (o- n m) m))))))

; division
(define div
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (div (o- n m) m))))))

(div 15 4)

; length
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(len '(hotdogs with mustard sauerkraut and pickles))
(len '(hame and cheese on rye))

; pick
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasanga spaghetti ravioli macaroni meatball))

; rempick
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n)
                       (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(number? 'tomato)
(number? 76)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (no-nums (cdr lat)))
      (else
        (cons (car lat)
              (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (eq a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(eqan? 1 1)
(eqan? 1 'tomato)
(eqan? 'tomato 'tomato)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a)
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur 'apples '(apples and apples and oranges))

; defintion of one (used simple version on first attempt)
(define one?
  (lambda (n)
    (eq n 1)))

(one? 1)
(one? 42)

; rempick refactored
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n)
                       (cdr lat)))))))

(rempick 3 '(lemon meringue salty pie))
