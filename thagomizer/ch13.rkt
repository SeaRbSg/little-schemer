#lang racket
(require rackunit)


(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (member? a (cdr lat))])))

;; (define intersect
;;   (lambda (s1 s2)
;;     (cond
;;      [(null? s1) '()]
;;      [(member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2))]
;;      [else (intersect (cdr s1) s2)])))

;; Letrec version
;; (define intersect
;;   (lambda (s1 s2)
;;     (letrec
;;         ((I (lambda (set)
;;               (cond
;;                [(null? set) '()]
;;                [(member? (car set) s2) (cons (car set) (I (cdr set)))]
;;                [else (I (cdr set))]))))
;;       (I s1))))

(define intersect
  (lambda (s1 s2)
    (letrec
        ((I (lambda (set)
              (cond
               [(null? set) '()]
               [(member? (car set) s2) (cons (car set) (I (cdr set)))]
               [else (I (cdr set))]))))
      (cond
       [(null? s2) s2]
       [else (I s1)]))))

(test-case "intersect"
           [check-equal? (intersect '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni)]
           [check-equal? (intersect '() '()) '()]
           [check-equal? (intersect '(a b c) '(d e f)) '()])


;; Chapter 7 version
;; (define intersectall
;;   (lambda (l-set)
;;     (cond
;;      [(null? l-set) l-set]
;;      [(null? (cdr l-set)) (car l-set)]
;;      [else (intersect (car l-set) (intersectall (cdr l-set)))])))

;; Letrec version
;; (define intersectall
;;   (lambda (lset)
;;     (letrec
;;         ((I 
;;           (lambda (lset)
;;             (cond
;;              [(null? (cdr lset)) (car lset)]
;;              [else (intersect (car lset) (I (cdr lset)))]))))
;;       (cond
;;        [(null? lset) '()]
;;        [else (I lset)]))))

;; Letcc version
;; (define intersectall
;;   (lambda (lset)
;;     (let/cc hop
;;            (letrec 
;;                ((I (lambda (lset)
;;                      (cond
;;                       [(null? (car lset)) (hop '())]
;;                       [(null? (cdr lset)) (car lset)]
;;                       [else (intersect (car lset) (I (cdr lset)))]))))
;;              (cond
;;               [(null? lset) '()]
;;               [else (I lset)])))))

;; Minor functions version
(define intersectall
  (lambda (lset)
    (let/cc hop
           (letrec 
               ((IA (lambda (lset)
                     (cond
                      [(null? (car lset)) (hop '())]
                      [(null? (cdr lset)) (car lset)]
                      [else (intersect (car lset) (IA (cdr lset)))])))
                (I (lambda (s1 s2)
                     (letrec
                         ((J (lambda (set)
                               (cond
                                [(null? set) '()]
                                [(member? (car set) s2) 
                                 (cons (car set) (I (cdr set)))]
                                [else (J (cdr set))]))))
                       (cond
                        [(null? s2) (hop '())]
                        [else (J s1)])))))
             (cond
              [(null? lset) '()]
              [else (IA lset)])))))


(test-case "intersectall"
           [check-equal? (intersectall '()) '()]
           [check-equal? (intersectall '((a))) '(a)]
           [check-equal? (intersectall '((a b) (a c))) '(a)]
           [check-equal? (intersectall '((a b) (a b c) (b c))) '(b)]
           [check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
                         '(a)]
           [check-equal? (intersectall '((6 pears and)
                                         (3 peaches and 6 peppers)
                                         (8 pears and 6 plums)
                                         (and 6 pruse with some apples)))
                         '(6 and)]
           [check-equal? (intersectall '((3 mangos and) (3 kiwis and) (3 hamburgers))) '(3)]
           [check-equal? (intersectall '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))) '()]
           [check-equal? (intersectall '((3 mangoes and) () (3 diet hamburgers))) '()])



(define rember 
  (lambda (a lat)
    (letrec 
        ((R (lambda (lat)
              (cond
               [(null? lat) '()]
               [(eq? (car lat) a) (cdr lat)]
               [else (cons (car lat) (R (cdr lat)))]))))
      (R lat))))

(test-case "rember"
           [check-equal? (rember 'mint '(lamb chops and mint jelly))
                      '(lamb chops and jelly)]
           [check-equal? (rember 'mint '(lamb chops and mint flavored jelly))
                      '(lamb chops and flavored jelly)]
           [check-equal? (rember 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato)]
           [check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea cup and hick cup)]
           [check-equal? (rember 'bacon '(bacon lettuce and tomato))
                      '(lettuce and tomato)]
           [check-equal? (rember 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato)]
           [check-equal? (rember 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce)])


(define rember-beyond-first
  (lambda (a lat)
    (letrec 
        ((R (lambda (lat)
              (cond
               [(null? lat) lat]
               [(eq? (car lat) a) '()]
               [else (cons (car lat) (R (cdr lat)))]))))
      (R lat))))

(test-case "rember-beyond-first"
           [check-equal? (rember-beyond-first 
                          'roots 
                          '(noodles spaghetti spatzle bean-thread 
                            roots potatoes yam others rice)) 
                         '(noodles spaghetti spatzle bean-thread)]
           [check-equal? (rember-beyond-first 
                          'others
                          '(noodles spaghetti spatzle bean-thread 
                            roots potatoes yam others rice)) 
                         '(noodles spaghetti spatzle bean-thread 
                           roots potatoes yam)]
           [check-equal? (rember-beyond-first 
                          'others
                          '(noodles spaghetti spatzle bean-thread 
                            roots potatoes yam others rice)) 
                         '(noodles spaghetti spatzle bean-thread 
                           roots potatoes yam)]
           [check-equal? (rember-beyond-first
                          'desserts
                          '(cookies chocolate mints caramel delight 
                            ginger snaps desserts chocolate mousse
                            vanilla ice cream German chocolate cake
                            more desserts gingerbreadman chocolate
                            chip brownies))
                         '(cookies chocolate mints caramel delight 
                           ginger snaps)])


(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
            (letrec
                ((R (lambda (lat)
                      (cond
                       [(null? lat) '()]
                       [(eq? (car lat) a) (skip (R (cdr lat)))]
                       [else (cons (car lat) (R (cdr lat)))]))))
              (R lat)))))

(test-case "rember-upto-last"
           [check-equal? (rember-upto-last 
                          'roots 
                          '(noodles spaghetti spatzle bean-thread
                            roots potatoes yam others rice))
                         '(potatoes yam others rice)]
           [check-equal? (rember-upto-last 
                          'sweetthing 
                          '(noodles spaghetti spatzle bean-thread
                            roots potatoes yam others rice))
                         '(noodles spaghetti spatzle bean-thread
                           roots potatoes yam others rice)]
           [check-equal? (rember-upto-last
                          'cookies
                          '(cookies chocolate mints caramel delight
                            ginger snaps desserts chocolate mousse
                            vanilla ice cream German chocolate cake
                            more cookies gingerbreadman chocolate chip
                            brownies))
                         '(gingerbreadman chocolate chip brownies)])
