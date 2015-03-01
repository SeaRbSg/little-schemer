#lang racket
(require rackunit)


(define x 
  (cons 'chicago
        (cons 'pizza '())))

(test-case "x"
           [check-equal? '(chicago pizza) x])

(set! x 'gone)   ;; MUTATION!!!!! ASSIGNMENT!!!! WOAH!!!!!

(test-case "x"
           [check-equal? 'gone x])

(set! x 'skins)


(test-case "x"
           [check-equal? 'skins x])

(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(test-case "gourmet"
           [check-equal? '(onion skins) (gourmet 'onion)])

(set! x 'rings)

(test-case "gourmet"
           [check-equal? '(onion rings) (gourmet 'onion)])


(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(test-case "gourmand"
           [check-equal? '(potato potato) (gourmand 'potato)]
           [check-equal? 'potato x]
           [check-equal? '(rice rice) (gourmand 'rice)]
           [check-equal? 'rice x])

(define diner
  (lambda (food)
    (cons 'milkshake
          (cons food '()))))

(define f '())
(define dinerR
  (lambda (food)
    (set! f food)
    (cons 'milkshake
          (cons f '()))))

(test-case "dinerR"
           [check-equal? '(milkshake onion) (dinerR 'onion)]
           [check-equal? '(milkshake pecanpie) (dinerR 'pecanpie)])

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))

;; I LOVE BOUILLABAISSE!!!! OM NOM NOM NOM 
(test-case "omnivore"
           [check-equal? '(bouillabaisse bouillabaisse) 
                         (omnivore 'bouillabaisse)])

(test-case "gourmand"
           [check-equal? 'rice x] ;; The same as above, the let x namespaces
           [check-equal? '(potato potato) (gourmand 'potato)]
           [check-equal? 'potato x]
           [check-equal? '(rice rice) (gourmand 'rice)]
           [check-equal? 'rice x])


(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food (cons x '())))))

(test-case "nibbler"
           [check-equal? '(cheerio cheerio) (nibbler 'cheerio)])


(define food 'none)
(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x '()))))))

(test-case "glutton"
           [check-equal? '(more garlic more garlic) (glutton 'garlic)])

(define chez-nous
  (lambda ()
    (let ((temp food))
      (set! food x)
      (set! x temp))))

(test-case "chez-nous"
           [check-equal? 'rice x]
           [check-equal? 'garlic food]
           (chez-nous)
           [check-equal? 'garlic x]
           [check-equal? 'rice food])
