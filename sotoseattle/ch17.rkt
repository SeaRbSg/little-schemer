#lang racket
(require "lib/shared.rkt")
(require rackunit)

;;; page 127

(define find  ; refactor with letrecc to abide by 12th commandment
  (lambda (n Ns Rs)
    (letrec
     ((A (lambda (enes erres)
         (cond
           [(null? enes) #f]
           [(eq? n (car enes)) (car erres)]
           [else (A (cdr enes) (cdr erres))]))))
     (A Ns Rs))))

;(define deepM3  ; from ch16 (more or less)
;  (let ((Ns '())
;        (Rs '()))
;    (letrec
;        ((D (lambda (m)
;                (if (zero? m) 
;                    'pizza 
;                    ;(cons (D (sub1 m)) '()))))) ; better to call deepM3 and take advantage of code
;                    (cons (deepM3 (sub1 m)) '()))))) 
;      (lambda (n)
;        (let ((gotit (find n Ns Rs))
;              (result (D n)))
;          (if gotit
;              gotit
;              (let ()
;                (set! Ns (cons n Ns))
;                (set! Rs (cons result Rs))
;                result)))))))

;;; page 128

;(define deepM  ; refactor a bit
;  (let ((Ns '())
;        (Rs '())
;        (D (lambda (m)
;             (if (zero? m) 
;                 'pizza 
;                 (cons (deepM (sub1 m)) '()))))) 
;      (lambda (n)
;        (let ((gotit (find n Ns Rs))
;              (result (D n)))
;          (if gotit
;              gotit
;              (let ()
;                (set! Ns (cons n Ns))
;                (set! Rs (cons result Rs))
;                result))))))

;;; page 129 & 130

; Consider that we can further simplify this:
;(let ((result 
;         ((lambda (m)
;            (if (zero? m) 
;                'pizza 
;                (cons (deepM (sub1 m)) '()))) n)))

(define deepM  ; more refactoring
  (let ((Ns '())
        (Rs '()))
      (lambda (n)
        (let ((gotit (find n Ns Rs)))
          (if gotit
              gotit
              (let ((result (if (zero? n) 
                               'pizza 
                               (cons (deepM (sub1 n)) '()))))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result))))))

(test-case "page 130"
    [check-equal? (deepM 3) '(((pizza)))]
    [check-equal? (deepM 5) '(((((pizza)))))]
    [check-equal? (deepM 3) '(((pizza)))])

;;; page 131

(define consC_v1
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

;;; page 132 & 133

(define counter 'hola)

(define consC                       ; define is run only once (not everytime consC is called)
  (let ((N 0))                      ; and therefore N is initialized to 0 only once !!!
    (set! counter (lambda () N))    ; like a getter method to N
    ;(set! counter N)               ; wrong because it sets it up to 0 in that moment. I want to get N instead
    (lambda (x y)
      (set! N (add1 N))             ; everytime consC is used => N adds 1
      (cons x y))))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m)) '()))))

(test-case "page 132 & 133"
    [check-equal? (deep 5) '(((((pizza)))))]
    [check-equal? (counter) 5]
    [check-equal? (deep 7) '(((((((pizza)))))))]
    [check-equal? (counter) 12])

;;; page 134

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(test-case "page 134"
    [check-equal? (supercounter deep) 500512])

;;; page 135

(define set-counter 'qwerty)

(set! consC  ; instead of define again
    (let ((N 0))
      (set! counter (lambda () N))
      (set! set-counter (lambda (x) (set! N x))) ; added this line
      (lambda (x y)
        (set! N (add1 N))
        (cons x y))))

(test-case "page 135"  
  ;(set-counter 0) ; actually it is not needed in this case because with the set! consC we made N == 0 again :-)
  [check-equal? (supercounter deep) 500500]
  (set-counter 0) ; now it is needed
  [check-equal? (supercounter deep) 500500])

;;; page 136 & 137

(set! deepM  ; more refactoring
  (let ((Ns '())
        (Rs '()))
      (lambda (n)
        (let ((gotit (find n Ns Rs)))
          (if gotit
              gotit
              (let ((result (if (zero? n) 
                               'pizza 
                               (consC (deepM (sub1 n)) '()))))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result))))))

(test-case "page 136 & 137"
  (set-counter 0)
  (deepM 5)
  [check-equal? (counter) 5]
  (deepM 7)
  [check-equal? (counter) 7]
  [check-equal? (supercounter deepM) 1000])

;;; page 138 & 139

(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                [(null? l) (oh 'no)]
                [(atom? (car l)) (if (eq? (car l) a)
                                     (cdr l)
                                     (consC (car l) (R (cdr l) oh)))]
                [else (let ((new-car
                             (let/cc oh
                               (R (car l) oh))))
                        (if (atom? new-car)
                            (consC (car l) (R (cdr l) oh))
                            (consC new-car (cdr l))))]))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(test-case "page 139"
  (set-counter 0)
  (rember1*C 'noodles '((food) more (food)))
  [check-equal? (counter) 0]) ; because there is no noodles in list

;;; page 140

(test-case "page 140"
  (set-counter 0)
  (consC (consC 'food '())
    (consC 'more
      (consC (consC 'food '())
        '())))
  [check-equal? (counter) 5])

(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                [(null? l) '()]
                [(atom? (car l)) (if (eq? (car l) a)
                                     (cdr l)
                                     (consC (car l) (R (cdr l))))]
                [else (let ((av (R (car l))))
                        (if (eqlist? (car l) av)
                            (consC (car l) (R (cdr l)))
                            (consC av (cdr l))))]))))
      (R l))))

;;; page 141

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)]
      [(null? l2) #f]
      [(eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))]
      [else #f])))

(test-case "page 141"
  (set-counter 0)
  (rember1*C2 'noodles '((food) more (food)))
  [check-equal? (counter) 5])
