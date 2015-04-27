#lang racket
(require rackunit)
(require miniKanren)

(define s# (== #f #f)) ;; succeed
(define u# (== #f #t)) ;; fail

;; First example on page 8. WTF?!?!
[check-equal? (run* (x)
                    (let ((x #f))
                      (fresh (x)
                             (== #t x))))
              '(_.0)]

;; Also how do you test against fresh variables?

[check-equal? (run* (r)
                   (fresh (x y)
                          (== (cons x (cons y '())) r)))
              '((_.0 _.1))]


[check-equal? (run* (s)
                    (fresh (t u)
                           (== (cons t (cons u '())) s)))
              '((_.0 _.1))]


[check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons y (cons x (cons y '()))) r)))))
              '((_.0 _.1 _.0))]

[check-equal? (run* (r)
                    (fresh (x)
                           (let ((y x))
                             (fresh (x)
                                    (== (cons x (cons y (cons x '()))) r)))))
              '((_.0 _.1 _.0))]

[check-equal? (run* (q)
                    (== #f q)
                    (== #t q))
              '()]


[check-equal? (run* (q)
                    (== #f q)
                    (== #f q))
              '(#f)]


[check-equal? (run* (q)
                    (let ((x q))
                      (== #t x)))
              '(#t)]


[check-equal? (run* (r)
                    (fresh (x)
                           (== x r)))
              '(_.0)]


[check-equal? (run* (q)
                    (fresh (x)
                           (== #t x)
                           (== x q)))
              '(#t)]


[check-equal? (run* (q)
                    (fresh (x)
                           (== x q)
                           (== #t x)))
              '(#t)]


[check-false (cond
               (#f #t)
               (else #f))]


;; I can't make (else u#) work. At all. 

(run* (x)
      (conde
       ((== 'olive x) s#)
       ((== 'oil x) s#)))
;; '(olive oil)


(run 1 (x)
     (conde
      ((== 'olive x) s#)
      ((== 'oil x) s#)))
;; '(olive)


(run* (x)
      (conde
       ((== 'virgin x) u#)
       ((== 'olive x) s#)
       (s# s#)
       ((== 'oil x) s#)))
;; '(olive _.0 oil)


(run 2 (x)
     (conde
      ((== 'extra x) s#)
      ((== 'virgin x) u#)
      ((== 'olive x) s#)
      ((== 'oil x) s#)))
;; '(extra olive)


(run* (r)
      (fresh (x y)
             (conde
              ((== 'split x) (== 'pea y))
              ((== 'navy x) (== 'bean y)))
             (== (cons x (cons y '())) r)))
;; '((split pea) (navy bean))


(run* (r)
      (fresh (x y)
             (conde
              ((== 'split x) (== 'pea y))
              ((== 'navy x) (== 'bean y)))
             (== (cons x (cons y (cons 'soup '()))) r)))
;; '((split pea soup) (navy bean soup))


(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) s#)
     ((== 'cup x) s#))))

[check-equal? (run* (x)
                   (teacupo x))
              '(tea cup)]


;; Review from here down

(run* (r)
      (fresh (x y)
             (conde
              ((teacupo x) (== #t y) s#)
              ((== #f x) (== #t y)))
             (== (cons x (cons y '())) r)))

(run* (r)
      (fresh (x y z)
             (conde
              ((== y x) (fresh (x) (== z x)))
              ((fresh (x) (== y x)) (== z x)))
             (== (cons y (cons z '())) r)))

(run* (r)
      (fresh (x y z)
             (conde 
              ((== y x) (fresh (x) (== z x)))
              ((fresh (x) (== y x)) (== z x)))
             (== #f x)
             (== (cons y (cons z '())) r)))

(run* (q)
      (let ((a (== #t q))
            (b (== #f q)))
        b))

(run* (q)
      (let ((a (== #t q))
            (b (fresh (x)
                      (== x q)
                      (== #f x)))
            (c (conde
                ((== #t q) s#)
                ((#t) (== #f q)))))
        b))
