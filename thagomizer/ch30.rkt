#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch27.rkt")

;; Previous Chapters
(define anyo               
  (lambda (g)
    (conde
     [g s#]
     [else (anyo g)])))

(define alwayso (anyo s#)) 

(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) s#)
     ((== 'cup x) s#))))

(define nevero (anyo u#))

(define salo
  (lambda (g)
    (conde
     [s# s#]
     [else g])))


;; 5
[check-equal? (run* (x)
                   (conda
                    [(== 'olive x) s#]
                    [(== 'oil x) s#]
                    [else u#]))
              '(olive)]

;; 7
[check-equal? (run* (x)
                    (conda
                     [(== 'virgin x) u#]
                     [(== 'olive x) s#]
                     [(== 'oil x) s#]
                     [else u#]))
              '()]

;; 8
[check-equal? (run* (q)
                    (fresh (x y)
                           (== 'split x)
                           (== 'pea y)
                           (conda
                            [(== 'split x) (== x y)]
                            [else s#]))
                    (== #t q))
              '()]

;; 9
[check-equal? (run* (q)
                    (fresh (x y)
                           (== 'split x)
                           (== 'pea y)
                           (conda
                            [(== x y) (== 'split x)]
                            [else s#]))
                    (== #t q))
              '(#t)]

;; 11
(define not-pastao
  (lambda (x)
    (conda 
     [(== 'pasta x) u#]
     [else s#])))

[check-equal? (run* (x)
                    (conda
                     [(not-pastao x) u#]
                     [else (== 'spaghetti x)]))
              '(spaghetti)]

;; 12
[check-equal? (run* (x)
                    (== 'spaghetti x)
                    (conda
                     [(not-pastao x) u#]
                     [else (== 'spaghetti x)]))
              '()]

;; 13
;; [check-equal? (run* (q)
;;                     (conda
;;                      [alwayso s#]
;;                      [eles u#]))
;;               '()]


;; 14
[check-equal? (run* (q)
                    (condu
                     [alwayso s#]
                     (else u#))
                    (== #t q))
              '(#t)]


;; 15
;; [check-equal? (run* (q)
;;                     (condu
;;                      [s# alwayso]
;;                      [else u#])
;;                     (== #t q))
;;               '()]


;; 17
;; [check-equal? (run 1 (q)
;;                    (conda
;;                     [alwayso s#]
;;                     [else u#])
;;                    u#
;;                    (== #t q))
;;               '()]


;; 18
[check-equal? (run 1 (q)
                   (condu
                    [alwayso s#]
                    [else u#])
                   u#
                   (== #t q))
              '()]

;; 19
(define onceo
  (lambda (g)
    (condu
     [g s#]
     [else u#])))

[check-equal? (run* (x)
                    (onceo (teacupo x)))
              '(tea)]


;; 20
[check-equal? (run 1 (q)
                   (onceo (salo nevero))
                   u#)
              '()]

;; 21
[check-equal? (run* (r)
                    (conde
                     [(teacupo r) s#]
                     [(== #f r) s#]
                     [else u#]))
              '(tea cup #f)]


;; 22
[check-equal? (run* (r)
                    (conda
                     [(teacupo r) s#]
                     [(== #f r) s#]
                     [else u#]))
              '(tea cup)]

;; 23
[check-equal? (run* (r)
                    (== #f r)
                    (conda
                     [(teacupo r) s#]
                     [(== #f r) s#]
                     [else u#]))
              '(#f)]

;; 24
[check-equal? (run* (r)
                    (== #f r)
                    (condu
                     [(teacupo r) s#]
                     [(== #f r) s#]
                     [else u#]))
              '(#f)]


;; 26
(define bumpo
  (lambda (n x)
    (conde
     [(== n x) s#]
     [else
      (fresh (m)
             (-o n '(1) m)
             (bumpo m x))])))

[check-equal? (run* (x)
                    (bumpo '(1 1 1) x))
              '((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ())]

;; 27
(define gen&testo
  (lambda (op i j k)
    (onceo
     (fresh (x y z)
            (op x y z)
            (== i x)
            (== j y)
            (== k z)))))

[check-equal? (run* (q)
                    (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
                    (== #t q))
              '(#t)]

;; 40
;; [check-equal? (run 1 (q)
;;                    (gen&testo +o '(0 0 1) '(1 1) '(0 1 1)))
;;               '()]


;; 43
(define enumerateo
  (lambda (op r n)
    (fresh (i j k)
           (bumpo n i)
           (bumpo n j)
           (op i j k)
           (gen&testo op i j k)
           (== `(,i ,j ,k) r))))

[check-equal? (run* (s)
                    (enumerateo +o s '(1 1)))
              '(((1 1) (1 1) (0 1 1))
                ((1 1) (0 1) (1 0 1))
                ((1 1) (1)   (0 0 1))
                ((1 1) ()    (1 1))
                ((0 1) (1 1) (1 0 1))
                ((0 1) (0 1) (0 0 1))
                ((0 1) (1)   (1 1))
                ((0 1) ()    (0 1))
                ((1)   (1 1) (0 0 1))
                ((1)   (0 1) (1 1))
                ((1)   (1)   (0 1))
                ((1)   ()    (1))
                (()    (1 1) (1 1))
                (()    (0 1) (0 1))
                (()    (1)   (1))
                (()    ()    ()))]

;; 56
[check-equal? (run 1 (s)
                   (enumerateo +o s '(1 1 1)))
              '(((1 1 1) (1 1 1) (0 1 1 1)))]


;; 57
(define gen-addero2
  (lambda (d n m r)
    (fresh (a b c e x y z)
           (== `(,a . ,x) n)
           (== `(,b . ,y) m) 
           (poso y)
           (== `(,c . ,z) r)
           (poso z)
           (all
            (full-addero d a b c e)
            (addero2 e x y z)))))

(define addero2
  (lambda (d n m r)
    (condi
     [(== 0 d) (== '() m) (== n r)]
     [(== 0 d) (== '() n) (== m r) (poso m)]
     [(== 1 d) (== '() m) (addero2 0 n '(1) r)]
     [(== 1 d) (== '() n) (poso m) (addero2 0 '(1) m r)]
     [(== '(1) n) (== '(1) m)
      (fresh (a c)
             (== `(,a ,c) r)
             (full-addero d 1 1 a c))]
     [(== '(1) n) (gen-addero2 d n m r)]
     [(== '(1) m) (>1o n) (>1o r) (addero2 d '(1) n r)]
     [(>1o n) (gen-addero2 d n m r)])))

(define +o2
  (lambda (n m k)
    (addero2 0 n m k)))


;; 58
;; [check-equal? (run 1 (q) (gen&testo +o2 '(0 1) '(1 1) '(1 0 1)))
;;               '()]
