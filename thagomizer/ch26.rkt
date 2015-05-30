#lang racket
(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")


(define anyo                ;; F1
  (lambda (g)
    (conde
     [g s#]
     [else (anyo g)])))

;; Yes it is recursive

(define nevero (anyo u#))   ;; F4

;; This never succeeds or fails

;; (run 1 (q) nevero (== #t q))

[check-equal? (run 1 (q)     ;; F6
                   u#
                   nevero)
              '()]

(define alwayso (anyo s#))   ;; F7

[check-equal? (run 1 (q)     ;; F7
                   alwayso
                   (== #t q))
              '(#t)]


[check-equal? (run 3 (q)   
                   alwayso
                   (== #t q))
              '(#t #t #t)]

;; This is infinite
;; (run* (q) alwayso (== #t q))

[check-equal? (run 5 (q)      ;; F10
                   alwayso
                   (== #t q))
              '(#t #t #t #t #t)]

[check-equal? (run 5 (q)      ;; F11
                   (== #t q)
                   alwayso)
              '(#t #t #t #t #t)]

(define salo                  ;; F12
  (lambda (g)
    (conde
     [s# s#]
     [else g])))

[check-equal? (run 1 (q)      ;; F13
                   (salo alwayso)
                   (== #t q))
              '(#t)]

[check-equal? (run 1 (q)      ;; F14
                   (salo nevero)
                   (== #t q))
              '(#t)]

;; Infinite
;; [check-equal? (run* (q) 
;;                     (salo nevero)
;;                     (== #t q))
;;               '()]


;; Infinite because run will try things until it comes up with a s#
;; The first clause of the conde in salo passes, it hits the u#, 
;; so it backtracks and tries the second clause in the conde in salo which
;; runs forever.
;; [check-equal? (run 1 (q)     ;; F16
;;                    (salo nevero)
;;                    u#
;;                    (== #t q))
;;               '()]

;; Also infinite
;; [check-equal? (run 1 (q)     ;; F17
;;                    alwayso
;;                    u#
;;                    (== #t q))
;;               '()]


;; Infinite
;; [check-equal? (run 1 (q)     ;; F18
;;                    (conde
;;                     [(== #f q) alwayso]
;;                     [else (anyo (== #t q))])
;;                    (== #t q))
;;               '()]

[check-equal? (run 1 (q)      ;; F19
                   (condi 
                    [(== #f q) alwayso]
                    [else (== #t q)])
                   (== #t q))
              '(#t)]

;; Infinite because [else (== #t q) has only one value that works so
;; we're stuck trying [(== #f q) alwayso] over and over
;; [check-equal? (run 2 (q)     ;; F20
;;                    (condi 
;;                     [(== #f q) alwayso]
;;                     [else (== #t q)])
;;                    (== #t q))
;;               '(#t)]


[check-equal? (run 5 (q)      ;; F21
                   (condi 
                    [(== #f q) alwayso]
                    [else (anyo (== #t q))])
                   (== #t q))
              '(#t #t #t #t #t)]

;; Compare condi and conde
;;;; Condi is similar to conde but it alternates between the clauses in
;;;; a fair way  clause 1 then clause 2 then clause 1 then clause 2 so
;;;; that one misbehaving clause can't take over

(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) s#)
     ((== 'cup x) s#))))

[check-equal? (run 5 (r)      ;; F24
                   (condi
                    [(teacupo r) s#]
                    [(== #f r) s#]
                    [else u#]))
              '(tea #f cup)]

[check-equal? (run 5 (r)      
                   (conde
                    [(teacupo r) s#]
                    [(== #f r) s#]
                    [else u#]))
              '(tea cup #f)]


[check-equal? (run 5 (q)      ;; F25
                   (condi 
                    [(== #f q) alwayso]
                    [(== #t q) alwayso]
                    [else u#])
                   (== #t q))
              '(#t #t #t #t #t)] ;; All from clause 2

;; Why does this have no value?
;; (run 5 (q)                   ;; F27
;;      (conde 
;;       [(== #f q) alwayso]
;;       [(== #t q) alwayso]
;;       [else u#])
;;      (== #t q))

;;;; It has no value because the first clause will generate an
;;;; infinite number of successes where q has been unified with #f,
;;;; which contradicts the last statement (== #t q) and so the whole
;;;; clause fails. It never gets to the second conde clause

[check-equal? (run 5 (q)      ;; F28
                  (conde
                   [alwayso s#]
                   [else nevero])
                  (== #t q))
             '(#t #t #t #t #t)]

;;;; If we replace conde with condi in frame 28 we will get an
;;;; infinite loop

;; (run 5 (q)                    ;; F30
;;      (condi 
;;       [always s#]
;;       [else nevero])
;;      (== #t q))


;; (run 1 (q)                    ;; F31
;;      (all 
;;       (conde
;;        [(== #f q) s#]
;;        [else (== #t q)])
;;       alwayso)
;;      (== #t q))
;;;; First q is unified with #f, then alwayso succeeds so the all
;;;; succeeds. After that we try to unify #t and q which fails, so we
;;;; backtrack and try the second value returned from alwayso, but q
;;;; is still unified with #f so (== #t q) fails again, and again, and
;;;; again.

[check-equal? (run 1 (q)       ;; F32
                    (alli
                     (conde
                      [(== #f q) s#]
                      [else (== #t q)])
                     alwayso)
                    (== #t q))
              '(#t)]

[check-equal? (run 5 (q)       ;; F33
                    (alli
                     (conde
                      [(== #f q) s#]
                      [else (== #t q)])
                     alwayso)
                    (== #t q))
              '(#t #t #t #t #t)]

[check-equal? (run 5 (q)       ;; F34
                    (alli
                     (conde
                      [(== #t q)]
                      [else (== #f q) s#])
                     alwayso)
                    (== #t q))
              '(#t #t #t #t #t)]

;; What does the i stand for in condi?
;;;; It stands for interleave.

[check-equal? (run 5 (q)       ;; F36
                   (all 
                    (conde
                     [s# s#]
                     [else nevero])
                    alwayso)
                   (== #t q))
              '(#t #t #t #t #t)] ;; All from the first clause of the conde

;; No value because once it hits nevero we don't get unwound
;; [check-equal? (run 5 (q)       ;; F37
;;                    (all 
;;                     (condi
;;                      [s# s#]
;;                      [else nevero])
;;                     alwayso)
;;                    (== #t q))
;;               '()]

;; Infinite loop for the same reasons as above
;; [check-equal? (run 5 (q)       ;; F38
;;                    (alli 
;;                     (conde
;;                      [s# s#]
;;                      [else nevero])
;;                     alwayso)
;;                    (== #t q))
;;               '()] 
