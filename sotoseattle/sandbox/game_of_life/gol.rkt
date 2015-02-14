#lang racket/base
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

; Cell Utility Functions ------------------------------------------------------

(define (x-coord cell) 
  (car cell))

(define (y-coord cell)
  (car (cdr cell)))

(define include?  ; does a board include a specific cell?
  (lambda (cell list)
    (letrec
      ((==? (lambda (c2) 
          (and (eq? (x-coord cell) (x-coord c2)) 
               (eq? (y-coord cell) (y-coord c2)))))
       (in? (lambda (l)
          (cond
            [(null? l) #f]
            [else (or (==? (car l)) (in? (cdr l)))]))))
      (in? list))))

(define uniq  ; remove duplicate cells in a list
  (lambda (lat)
    (cond
      [(null? lat) lat]
      [(include? (car lat) (cdr lat)) (uniq (cdr lat))]
      [else (cons (car lat) (uniq (cdr lat)))])))

(define adj  ; 8 adjacent neighboring cells around a cell
  (lambda (cell)
    (define x (x-coord cell))
    (define y (y-coord cell))
    (cons (cons (+ x 1) (cons (- y 1) '()))
    (cons (cons (+ x 1) (cons y '()))
    (cons (cons (+ x 1) (cons (+ y 1) '()))
    (cons (cons x (cons (- y 1) '()))
    (cons (cons x (cons (+ y 1) '()))
    (cons (cons (- x 1) (cons (- y 1) '()))
    (cons (cons (- x 1) (cons y '()))
    (cons (cons (- x 1) (cons (+ y 1) '())) '()))))))))))

(define n-adj-alive  ; count the number of living neighbors around a cell
  (lambda (cell living)
    (letrec
      ((rec (lambda (n cells)
          (cond
            [(null? cells) n]
            [(include? (car cells) living) (rec (+ n 1) (cdr cells))]
            [else (rec n (cdr cells))]))))
      (rec 0 (adj cell)))))

; Rule 1: when a cell will remains alive after a tick of the clock ------------

(define stay-alive
  (letrec
     ((rec (lambda (board living new-board)
            (letrec
              ((survivors
                (letrec
                    ((alive? (lambda (cell)
                        (cond
                          [(eq? 2 (n-adj-alive cell living)) #t]
                          [(eq? 3 (n-adj-alive cell living)) #t]
                          [else #f]))))
                    (lambda (old new)
                      (cond
                        [(null? old) new]
                        [(alive? (car old)) (survivors (cdr old) (cons (car old) new))]
                        [else (survivors (cdr old) new)])))))
              (survivors board new-board)))))
     (lambda (board)
       (rec board board '()))))

; Rule 2: when a dead cell becomes alive after a tick of the clock ------------

(define become-alive
  (lambda (living)
    (letrec
      ((perimeter (lambda (alive potentials)
         (letrec
             ((cell-cons
               (lambda (l1 l2)
                 (cond
                   [(null? l1) l2]
                   [(include? (car l1) living) (cell-cons (cdr l1) l2)]
                   [else (cell-cons (cdr l1) (cons (car l1) l2))]))))
             (cond
               [(null? alive) potentials]
               [else (perimeter (cdr alive) (cell-cons (adj (car alive)) potentials))])
           )))
       (germinate
          (letrec
              ((rec (lambda (potentials)
                 (letrec
                     ((raise? (lambda (cell)
                         (cond
                           [(eq? 3 (n-adj-alive cell living)) #t]
                           [else #f]))))
                   (cond
                     [(null? potentials) potentials]
                     [(raise? (car potentials)) (cons (car potentials) (germinate (cdr potentials)))]
                     [else (germinate (cdr potentials))])))))
              (lambda (maybe)
                (rec maybe)))))
      (germinate (uniq (perimeter living '()))))))

;;;;;;;;; TESTS 

(module+ test
  (check-equal? (uniq '((0 1))) '((0 1)))
  (check-equal? (uniq '((0 -1) (1 1) (1 -1) (2 1) (2 0) (2 -1) (-1 2)
                        (-1 1) (-1 0) (0 2) (1 2) (1 1) (-1 1) (-1 0)
                        (-1 -1) (0 -1) (1 1) (1 -1)))
                      '((2 1) (2 0) (2 -1) (-1 2) (0 2) (1 2) (-1 1) (-1 0)
                        (-1 -1) (0 -1) (1 1) (1 -1)))
  (check-equal? (adj '(0 0)) '((1 -1) (1 0) (1 1) (0 -1) (0 1) (-1 -1) (-1 0) (-1 1)))
  [check-true   (include? '(1 1) '((0 0) (2 1) (1 1) (-1 -1)))]
  [check-true   (include? '(1 1) '((1 1)))]
  [check-false  (include? '(1 1) '())]
  [check-false  (include? '(1 1) '((2 2)))]
  [check-equal? 2 (n-adj-alive '(0 0) '((0 0) (2 1) (1 1) (-1 -1)))]
  [check-equal? 8 (n-adj-alive '(0 0) '((1 -1) (1 0) (1 1) (0 -1) (0 1) (-1 -1) (-1 0) (-1 1)))]
  (check-equal? (stay-alive '((0 0) (0 1) (1 0))) '((1 0) (0 1) (0 0)))
  (check-equal? (stay-alive '((0 0) (0 1) (0 2))) '((0 1)))
  (check-equal? (become-alive '((0 0) (0 1) (0 2))) '((-1 1) (1 1)))
  (check-equal? (become-alive '((0 0) (0 1) (1 0))) '((1 1))))

; VISUALIZATION ---------------------------------------------------------------

(define SEED '((6 18)  (6 19)  (6 31)  (6 32)  (7 18)  (7 19)  (7 31)  (7 32)  (10 13) (10 37) (11 12)
               (11 14) (11 20) (11 30) (11 36) (11 38) (12 11) (12 14) (12 20) (12 22) (12 23)
               (12 27) (12 28) (12 30) (12 36) (12 39) (13 12) (13 13) (13 24) (13 26) (13 37)
               (13 38) (14 22) (14 24) (14 26) (14 28) (15 23) (15 27) (17 7)  (17 8)  (17 42) (17 43)
               (18 7)  (18 8)  (18 42) (18 43) (19 12) (19 13) (19 37) (19 38) (21 13) (21 15) (21 35)
               (21 37) (22 13) (22 16) (22 34) (22 37) (23 14) (23 15) (23 35) (23 36) (25 14) (25 15)
               (25 35) (25 36) (26 13) (26 16) (26 34) (26 37) (27 13) (27 15) (27 35) (27 37) (29 12)
               (29 13) (29 37) (29 38) (30 7)  (30 8)  (30 42) (30 43) (31 7)  (31 8)  (31 42) (31 43)
               (33 23) (33 27) (34 22) (34 24) (34 26) (34 28) (35 12) (35 13) (35 24) (35 26) (35 37)
               (35 38) (36 11) (36 14) (36 20) (36 22) (36 23) (36 27) (36 28) (36 30) (36 36) (36 39)
               (37 12) (37 14) (37 20) (37 30) (37 36) (37 38) (38 13) (38 37) (41 18) (41 19) (41 31)
               (41 32) (42 18) (42 19) (42 31) (42 32)))
(define WIDTH 800)
(define SIDE 16)
(define COLOR "firebrick")
(define GREYISH (make-color  245  245 245))
(define WTF (rectangle (- SIDE 1) (- SIDE 1) "solid" COLOR))

(define tick-of-the-clock
  (letrec
      ((+ (lambda (l1 l2)
           (cond
             [(null? l1) l2]
             [else (cons (car l1) (+ (cdr l1) l2))]))))
      (lambda (render_board)
        (uniq (+ (become-alive render_board) (stay-alive render_board))))))

(define draw-board
  (lambda (living-cells)
    (cond
      [(null? living-cells) (empty-scene WIDTH WIDTH GREYISH)]
      [else (place-image WTF
                         (* (x-coord (car living-cells)) SIDE)
                         (* (y-coord (car living-cells)) SIDE)
                         (draw-board (cdr living-cells)))])))

(big-bang SEED
          (on-tick tick-of-the-clock 0.3)
          (to-draw draw-board))
