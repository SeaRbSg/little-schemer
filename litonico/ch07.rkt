(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t] ; empty set
      [(member? (car lat) (cdr lat))]
      [else (set? (cdr lat))])))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
      [else (cons (car lat) (makeset (cdr lat)))])))

(define makeset1
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car lat) 
                  (makeset (multirember (car lat) (cdr lat))))])))

(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [(member? (car set1) set2) (subset? (cdr set1) set2)]
      [else #f])))

(define eqset?
  (lambda (set1 set2)
    (cond
      [(subset? set1 set2) (subset? set2 set1)]
      [else #f])))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))])))

(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))])))


;; That function is the 'relative complement' ('difference')

(define intersectall
  (lambda (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set) (intersectall (cdr l-set)))])))

(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(define first ;; why the heck do they use `(cond [else])`?
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (cadr x)))

(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (caar l)
                  (firsts (cdr l)))])))

(define seconds
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (cdar l)
                  (firsts (cdr l)))])))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (build
                    (second (car rel))
                    (first (car rel)))
                  (revrel (cdr rel)))])))
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
