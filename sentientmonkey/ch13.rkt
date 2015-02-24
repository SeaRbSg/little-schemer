#lang racket/base

(require rackunit)
(require "prelude.rkt")
(require "ch02.rkt")

(define (intersect set1 set2)
  (letrec
    ([I (lambda (set1)
          (cond
            [(null? set1) '()]
            [(member? (car set1) set2)
             (cons (car set1) (I (cdr set1)))]
            [else (I (cdr set1))]))])
    (I set1)))

(test-case "intersect"
    (check-equal? (intersect '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni)))

(define (intersectall-1 lset)
  (cond
    [(null? lset) '()]
    [(null? (cdr lset)) (car lset)]
    [else (intersect (car lset)
                     (intersectall-1 (cdr lset)))]))

(define (test-intersect-all intersectall)
  (test-case "intersectall"
     (check-equal? (intersectall '((3 mangos and)
                                   (3 kiwis and)
                                   (3 hamburgers))) '(3))
     (check-equal? (intersectall '((3 steaks and)
                                   (no food and)
                                   (three backed potatoes)
                                   (3 diet hamburgers))) '())
     (check-equal? (intersectall '((3 mangos and)
                                   ()
                                   (3 diet hamburgers))) '())
     (check-equal? (intersectall '()) '())))

(test-intersect-all intersectall-1)

(define (intersectall-2 lset)
  (letrec
    ([A (lambda (lset)
          (cond
            [(null? (cdr lset)) (car lset)]
            [else (intersect (car lset)
                             (A (cdr lset)))]))])
    (cond
      [(null? lset) '()]
      [else (A lset)])))


(test-intersect-all intersectall-2)

(define (intersectall-3 lset)
  (let/cc hop
      (letrec
        ([A (lambda (lset)
              (cond
                [(null? lset) (hop '())]
                [(null? (cdr lset)) (car lset)]
                [else (intersect (car lset)
                                 (A (cdr lset)))]))])
        (cond
          [(null? lset) '()]
          [else (A lset)]))))

(test-intersect-all intersectall-3)

(define (intersectall-4 lset)
  (let/cc hop
      (letrec
        ([A (lambda (lset)
              (cond
                [(null? lset) (hop '())]
                [(null? (cdr lset)) (car lset)]
                [else (I (car lset)
                         (A (cdr lset)))]))]
         [I (lambda (set1 set2)
              (letrec
                ([J (lambda (set1)
                      (cond
                        [(null? set1) '()]
                        [(member? (car set1) set2)
                         (cons (car set1) (J (cdr set1)))]
                        [else (J (cdr set1))]))])
                (cond
                  [(null? set2) (hop '())]
                  [else (J set1)])))])
        (cond
          [(null? lset) '()]
          [else (A lset)]))))

(test-intersect-all intersectall-4)

(define (rember a lat)
  (letrec
    ([R (lambda (lat)
          (cond
            [(null? lat) '()]
            [(eq? (car lat) a) (cdr lat)]
            [else (cons (car lat)
                        (R (cdr lat)))]))])
    (R lat)))

(test-case "rember"
   (check-equal? (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (check-equal? (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce)))

(define (rember-beyond-first a lat)
  (letrec
    ([R (lambda (lat)
          (cond
            [(null? lat) '()]
            [(eq? (car lat) a) '()]
            [else (cons (car lat)
                        (R (cdr lat)))]))])
    (R lat)))

(define starches '(noodles
                    spaghetti spätzle bean-thread
                    roots
                    potatoes yam
                    others
                    rice))

(define desserts '(cookies
                    chocolate mints
                    caramel delight ginger snaps
                    desserts
                    chocolate mousse
                    vanilla ice cream
                    German chocolate cake
                    more desserts
                    gingerbreadman chocolate chip brownies))

(test-case "rember-beyond-first"
   (check-equal? (rember-beyond-first 'roots starches)
                 '(noodles spaghetti spätzle bean-thread))

   (check-equal? (rember-beyond-first 'others starches)
                 '(noodles
                    spaghetti spätzle bean-thread
                    roots
                    potatoes yam))

   (check-equal? (rember-beyond-first 'sweetthing starches) starches)

   (check-equal? (rember-beyond-first 'desserts desserts)
                 '(cookies
                    chocolate mints
                    caramel delight ginger snaps)))


(define (rember-upto-last a lat)
  (let/cc skip
      (letrec
        ([R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) (skip (R (cdr lat)))]
                [else (cons (car lat)
                            (R (cdr lat)))]))])
        (R lat))))

(test-case "rember-upto-last"
   (check-equal? (rember-upto-last 'roots starches)
                 '(potatoes yam
                            others
                            rice))

   (check-equal? (rember-upto-last 'sweetthing starches) starches)

   (check-equal? (rember-upto-last 'desserts desserts)
                 '(gingerbreadman chocolate chip brownies))

   (check-equal? (rember-upto-last 'cookies desserts)
                 '(chocolate mints
                             caramel delight ginger snaps
                             desserts
                             chocolate mousse
                             vanilla ice cream
                             German chocolate cake
                             more desserts
                             gingerbreadman chocolate chip brownies)))

