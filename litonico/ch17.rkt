(define find
  (lambda (n Ns Rs)
    (eltrec
      ((A (lambda (ns rs)
            (cond
              [(= (car ns) n) (car rs)]
              [else
                (A (cdr ns) (cdr rs))]))))
      (A Ns Rs))))

(define deep
  (lambda (m)
	(if (zero? m)
	    'pizza
	    (consC (deep (sub1 m))
		   '()))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n) 'pizza
                              (cons (deepM (sub1n))
                                    '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))


(define counter)

(define consC2
  (let ((N 0))
    (set! counter
      (lambda ()
        N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))


(set! deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n) 'pizza
                              (consC (deepM (sub1n))
                                    '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

