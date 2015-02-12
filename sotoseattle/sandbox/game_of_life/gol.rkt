#lang racket/base
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)


; Cell Utility Functions ------------------------------------------------------

(define (x-coord cell)
    (car cell))

(define (y-coord cell)
    (car (cdr cell)))

(define (same-cell? c1 c2)
    (and (eq? (x-coord c1) (x-coord c2)) (eq? (y-coord c1) (y-coord c2))))

(define uniq  ; remove duplicate cells in a list
  (lambda (lat)
    (cond
      [(null? lat) lat]
      [(include? (car lat) (cdr lat)) (uniq (cdr lat))]
      [else (cons (car lat) (uniq (cdr lat)))])))

  (module+ test
    (check-equal? (uniq '((0 1))) '((0 1)))
    (check-equal? (uniq '((0 -1) (1 1) (1 -1) (2 1) (2 0) (2 -1) (-1 2)
                          (-1 1) (-1 0) (0 2) (1 2) (1 1) (-1 1) (-1 0)
                          (-1 -1) (0 -1) (1 1) (1 -1)))
                  '((2 1) (2 0) (2 -1) (-1 2) (0 2) (1 2) (-1 1) (-1 0)
                    (-1 -1) (0 -1) (1 1) (1 -1))))

(define add-lats  ; just add two lists of cells
  (lambda (lat1 lat2)
    (cond
      [(null? lat1) lat2]
      [else (cons (car lat1) (add-lats (cdr lat1) lat2))])))

; 8 neighboring cells around a cell -------------------------------------------

(define neighbors
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

  (module+ test
    (check-equal? (neighbors '(0 0)) '((1 -1) (1 0) (1 1) (0 -1) (0 1) (-1 -1) (-1 0) (-1 1))))

; does a board include a specific cell? ---------------------------------------

(define include?
  (lambda (cell board)
    (cond
      [(null? board) #f]
      [else (or (same-cell? cell (car board)) (include? cell (cdr board)))])))

  (module+ test
    [check-true (include? '(1 1) '((0 0) (2 1) (1 1) (-1 -1)))]
    [check-true (include? '(1 1) '((1 1)))]
    [check-false (include? '(1 1) '())]
    [check-false (include? '(1 1) '((2 2)))]
    )

; count the number of living neighbors around a cell --------------------------

(define living-neigbors
  (lambda (cell living)
    (living-neighbors-b 0 (neighbors cell) living)))

  (define living-neighbors-b
    (lambda (n cells living)
      (cond
        [(null? cells) n]
        [(include? (car cells) living) (living-neighbors-b (+ n 1) (cdr cells) living)]
        [else (living-neighbors-b n (cdr cells) living)])))

  (module+ test
    [check-equal? 2 (living-neigbors '(0 0) '((0 0) (2 1) (1 1) (-1 -1)))]
    [check-equal? 8 (living-neigbors '(0 0) '((1 -1) (1 0) (1 1) (0 -1) (0 1) (-1 -1) (-1 0) (-1 1)))])

; Rule 1: say if a cell will be alive after a tick of the clock ---------------

(define alive?
  (lambda (cell board)
    (cond
      [(or (eq? 2 (living-neigbors cell board)) (eq? 3 (living-neigbors cell board))) #t]
      [else #f])))

; new list with living cells after a tick (only apply Rule 1) -----------------

(define staying-alive
  (lambda (board)
    (staying-alive-b board board '())))

  (define staying-alive-b
    (lambda (board living new_board)
      (cond
        [(null? board) new_board]
        [(alive? (car board) living) (staying-alive-b (cdr board) living (cons (car board) new_board))]
        [else (staying-alive-b (cdr board) living new_board)])))

  (module+ test
    (check-equal? (staying-alive '((0 0) (0 1) (1 0))) '((1 0) (0 1) (0 0)))
    (check-equal? (staying-alive '((0 0) (0 1) (0 2))) '((0 1))))

; Rule 2: say if a dead cell will be alive after a tick of the clock ----------

(define raise?
  (lambda (cell board)
    (cond
      [(eq? 3 (living-neigbors cell board)) #t]
      [else #f])))

; get the perimeter of all living cells ---------------------------------------

(define perimeter-cells
  (lambda (board potentials living)
    (cond
      [(null? board) potentials]
      [else (perimeter-cells (cdr board)
                               (cell-cons (neighbors (car board)) potentials living)
                               living)])))

; special cons for cells removing living ones ---------------------------------

(define cell-cons
  (lambda (latofcells lat living)
    (cond
      [(null? latofcells) lat]
      [(include? (car latofcells) living) (cell-cons (cdr latofcells) lat living)]
      [else (cell-cons (cdr latofcells) (cons (car latofcells) lat) living)])))

(define potential-cells
  (lambda (board)
    (uniq (perimeter-cells board '() board))))

(define new-living-cells
  (lambda (board)
    (new-living-cells-b (potential-cells board) board)))

  (define new-living-cells-b
    (lambda (potentials living)
    (cond
      [(null? potentials) potentials]
      [(raise? (car potentials) living) (cons (car potentials) (new-living-cells-b (cdr potentials) living))]
      [else (new-living-cells-b (cdr potentials) living)])))

  (module+ test
    (check-equal? (new-living-cells '((0 0) (0 1) (0 2))) '((-1 1) (1 1)))
    (check-equal? (new-living-cells '((0 0) (0 1) (1 0))) '((1 1))))

; Both rules together to create the new board ---------------------------------
; uncomment to use

;(define time-passes
;  (lambda (n board)
;    (cond
;      [(zero? n) (print 'bye)]
;      [else (and
;             (print board)
;             (time-passes (- n 1)
;                          (uniq (add-lats (new-living-cells board)
;                                          (staying-alive board)))))])))
;
;(time_passes 10 '((0 0) (0 1) (0 2)))
;(time_passes 3 '((0 0) (0 1) (1 0)))

; VISUALIZATION ---------------------------------------------------------------

(define tick-of-the-clock
  (lambda (render_board)
    (uniq (add-lats (new-living-cells render_board) (staying-alive render_board)))))

(define COLOR "firebrick")
(define WIDTH 800)
(define SIDE 16)
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
(define WTF (rectangle (- SIDE 1) (- SIDE 1) "solid" COLOR))

(define draw-board-on-empty-scene
  (lambda (living-cells)
    (cond
      [(null? living-cells) (empty-scene WIDTH WIDTH (make-color  245  245 245))]
      [else (place-image WTF
                         (* (x-coord (car living-cells)) SIDE)
                         (* (y-coord (car living-cells)) SIDE)
                         (draw-board-on-empty-scene (cdr living-cells)))])))

(big-bang SEED
          (on-tick tick-of-the-clock 0.3)
          (to-draw draw-board-on-empty-scene))

