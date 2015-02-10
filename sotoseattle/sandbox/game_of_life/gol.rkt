#lang racket
(require rackunit)
;(require racket/trace)

; 8 neighboring cells around a cell
(define neighbors
  (lambda (cell)
    (define x (x_coord cell))
    (define y (y_coord cell))
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

(define x_coord
  (lambda (cell)
    (car cell)))

(define y_coord
  (lambda (cell)
    (car (cdr cell))))

(define same-cell?
  (lambda (c1 c2)
    (and (eq? (x_coord c1) (x_coord c2)) (eq? (y_coord c1) (y_coord c2)))))

; does a board include a specific cell?
(define include?
  (lambda (cell board)
    (cond
      [(null? board) #f]
      [else (or (same-cell? cell (car board))
                (include? cell (cdr board)))])))

(module+ test
  [check-true (include? '(1 1) '((0 0) (2 1) (1 1) (-1 -1)))]
  [check-true (include? '(1 1) '((1 1)))]
  [check-false (include? '(1 1) '())]
  [check-false (include? '(1 1) '((2 2)))]
  )

; count the number of living neighbors around a cell
(define living-neighbors-b
  (lambda (n cells living)
    (cond
      [(null? cells) n]
      [else 
       (cond
         [(include? (car cells) living) (living-neighbors-b (+ n 1) (cdr cells) living)]
         [else (living-neighbors-b n (cdr cells) living)])])))

(define living-neigbors
  (lambda (cell living)
    (living-neighbors-b 0 (neighbors cell) living)))

(module+ test
  [check-equal? 2 (living-neigbors '(0 0) '((0 0) (2 1) (1 1) (-1 -1)))]
  [check-equal? 8 (living-neigbors '(0 0) '((1 -1) (1 0) (1 1) (0 -1) (0 1) (-1 -1) (-1 0) (-1 1)))])

; Rule 1: say if a cell will be alive after a tick of the clock
(define alive?
  (lambda (cell board)
    (cond
      [(or (eq? 2 (living-neigbors cell board))
           (eq? 3 (living-neigbors cell board))) #t]
      [else #f])))

; new list with living cells after a tick (only apply Rule 1)
(define staying-alive-b
  (lambda (board living new_board)
    (cond
      [(null? board) new_board]
      [(alive? (car board) living) (staying-alive-b (cdr board) living (cons (car board) new_board))]
      [else (staying-alive-b (cdr board) living new_board)])))

(define staying-alive
  (lambda (board)
    (staying-alive-b board board '())))

(module+ test
  (check-equal? (staying-alive '((0 0) (0 1) (1 0))) '((1 0) (0 1) (0 0)))
  (check-equal? (staying-alive '((0 0) (0 1) (0 2))) '((0 1))))
    
; Rule 2: say if a dead cell will be alive after a tick of the clock
(define raise?
  (lambda (cell board)
    (cond
      [(eq? 3 (living-neigbors cell board)) #t]
      [else #f])))

(define perimeter-cells-b ; get the perimeter of all living cells
  (lambda (board potentials living)
    (cond
      [(null? board) potentials]
      [else (perimeter-cells-b (cdr board) 
                               (cell-cons (neighbors (car board)) potentials living) 
                               living)])))

(define uniq ; remove duplicate cells in a list
  (lambda (lat)
    (cond
      [(null? lat) lat]
      [else
       (cond
         [(include? (car lat) (cdr lat)) (uniq (cdr lat))]
         [else (cons (car lat) (uniq (cdr lat)))])])))

(module+ test
  (check-equal? (uniq '((0 1))) '((0 1)))
  (check-equal? (uniq '((0 -1) (1 1) (1 -1) (2 1) (2 0) (2 -1) (-1 2) (-1 1) (-1 0) (0 2) (1 2) (1 1) 
                        (-1 1) (-1 0) (-1 -1) (0 -1) (1 1) (1 -1)))
                '((2 1) (2 0) (2 -1) (-1 2) (0 2) (1 2) (-1 1) (-1 0) (-1 -1) (0 -1) (1 1) (1 -1))))

(define cell-cons ; special cons for cells removing living ones
  (lambda (latofcells lat living)
    (cond
      [(null? latofcells) lat]
      [else 
       (cond
         [(include? (car latofcells) living) (cell-cons (cdr latofcells) lat living)]
         [else (cell-cons (cdr latofcells) (cons (car latofcells) lat) living)])])))
       
(define potential-cells
  (lambda (board)
    (uniq (perimeter-cells-b board '() board))))

(define new-living-cells-b
  (lambda (potentials living)
  (cond
    [(null? potentials) potentials]
    [(raise? (car potentials) living) (cons (car potentials) (new-living-cells-b (cdr potentials) living))]
    [else (new-living-cells-b (cdr potentials) living)])))

(define new-living-cells
  (lambda (board)
    (new-living-cells-b (potential-cells board) board)))

(module+ test
  (check-equal? (new-living-cells '((0 0) (0 1) (0 2))) '((-1 1) (1 1)))
  (check-equal? (new-living-cells '((0 0) (0 1) (1 0))) '((1 1))))

; Both rules together to create the new board
(define add-lats
  (lambda (lat1 lat2)
    (cond
      [(null? lat1) lat2]
      [else (cons (car lat1) (add-lats (cdr lat1) lat2))])))

(define tick
  (lambda (n board)
    (cond
      [(zero? n) (print 'bye)]
      [else (and
             (pretty-print board)
             (tick (- n 1) (uniq (add-lats (new-living-cells board) (staying-alive board)))))])))

;(tick 10 '((0 0) (0 1) (0 2)))
;(tick 3 '((0 0) (0 1) (1 0)))
