; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))

'atom
'turkey
'1492
'u
'*abc$

(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdogs)) (and) (pickle) relish)))
(cdr '(a b c))
(cdr '(hamburger))
(cdr '((x) t r))
(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))

(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

(null? (quote ()))
(null? '(a b c))

(atom? 'Harry)
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry))
(eq? '6 '7)
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured mlik)) 'milk)

(eq? (cdr '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))

