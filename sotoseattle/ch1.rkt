; ______________INITIAL DEFINITIONS______________
#lang racket
; from preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not(null? x)))))
; make sure atom? is defined #=> should output #f
(atom? (quote()))

; ______________ATOMS______________
'atom
'turkey
'1942
'u
'*abc$

; ______________LISTS______________
'(atom)
'(atom turkey or)
'((atom turkey) or)
'(how are you doing so far)

; ______________S Expression______________
'xyz
'(x y z)
'((x y) z)

; a list is a collection of S-expressions inside parenthesis (level 1)
'(how are you doing so far)            ; # S-exp = 6 atoms
'(((how) are) ((you) (doing so)) far)  ; # S-exp = 1 atom and 2 lists
'()            ; is a list with no S-exp inside. This S-exp is called the null or empty list
'(() () () ()) ; is a list with 4 S-exp (lists) inside

; ______________CAR______________
; first element of a list, returns an element of the list (list or atom)
(car '(a b c))        ; 'a
(car '((a b c) x y))  ; '(a b c)
; (car 'atom) => error, only works on lists
; (car ()) => error THE PRIMITIVE LAW OF CAR!!!
(car '(((hotdogs)) (and) (pickle) relish))             ; => ((hotdogs)) the list of the list of hotdogs
(car (car '(((hotdogs)) (and) (pickle) relish)))       ; => (hotdogs)
(car (car (car '(((hotdogs)) (and) (pickle) relish)))) ; => 'hotdogs

; ______________CDR______________
; could'er => the list without car (the list!), returns a list
(cdr '(a b c))
(cdr '((a b c) x y))
(cdr '(hamburger))
(cdr '((x) t r))
; (cdr 'atom) => error, only works on lists
; (cdr ()) => error THE PRIMITIVE LAW OF CDR!!!

(car (cdr '((b) (x y) ((c)))))  ; from the inside out, 1sr cdr => ((x y) ((c))), 2nd car => (x y)
(cdr (cdr '((b) (x y) ((c)))))  ; from the inside out, 1sr cdr => ((x y) ((c))), 2nd cdr => (((c))) <== list of (( c ))
;(cdr (car '(a (b (c)) d))) ;=> error because cdr of atom is not allowed

; both car and cdr take as argument a non-empty list
; car returns an S-exp, cdr returns a list

; ______________CONS______________
; takes 2 arguments and adds an S-exp to the front of a list
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '()) ; interesting, works too
(cons 's '())
(cons '((a b c)) 'b) ; => should be error because 2nd arg is not a list ############# CHEK
(cons 'a 'b)         ; => should be error because 2nd arg is not a list ############# CHEK
; LAW OF CONS: takes 2 args, the second must be a list, returns a list

(cons 'a (car '((b) c d))) ; ############# CHEK messy way to add ' to S-expr but not to (car / (cdr / (cons
(cons 'a (cdr '((b) c d)))

; ______________NULL?______________
(null? '())
(null? '(a b c))
(null? 'a)  ; should be no answer, but gives false because it is always #f except for the empty list
; LAW OF NULL?: is defined only for a list

; ______________ATOM?______________
; atom? takes a single argument, a S-exp
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr '(swing (low sweet) cheery oat))))

; ______________EQ?______________
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry))
(eq? '() '())
(eq? '(pepe) '(pepe))
(eq? '6 '7)
(eq? '6 '6)
; LAW OF EQ?: takes two non-numeric atoms as arguments
; ########### CHECK works with empty lists and gives #t (for anything else gives #f)
; ########### CHECK works with numbers as strings
(eq? (car '(Mary had a litle lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)  ; shouldn't give an answer, comparing non-atom '(milk) with 'milk
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))
