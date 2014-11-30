#lang scribble/lp

While reading @italic{The Little Schemer} I'll record my notes in this literate program.

@section{Toys}

Fundamental building blocks of the Scheme grammar are introduced here.

@itemlist[
  @item{atoms}
  @item{lists}
  @item{s-expressions}
  @item{operations}
  @item{arguments}]

Additionally, several operations and keywords are explored.

@itemlist[
  @item{@tt{atom?}}
  @item{@tt{car}}
  @item{@tt{cdr}}
  @item{@tt{cons}}
  @item{@tt{null?}}
  @item{@tt{quote}}
  @item{@tt{eq?}}]

Unsurprisingly, operations ending with @tt{?} are asking a question, specifically a yes or no question. For example, @tt{(atom? @italic{x})} asks, "is @italic{@tt{x}} an atom?"

What's an atom anyway? According to the definition of @tt{atom?} provided in the preface it's anything that's neither a pair nor null.

@chunk[<atom?-definition>
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))]

Okay, what's a pair? Intuitively, it's something combined with something else. In Scheme things can be combined using the @italic{@tt{cons}} operation. According to @italic{The Law of Cons}, it takes two arguments and the second must be a list. The result is a list.

The most basic list is the empty list: @tt{'()}, which can also written as @tt{(@bold{quote} ())}. All other lists have two parts, a @tt{car} (aka head) and a @tt{cdr} (aka tail), but the empty list has only one part, namely itself. For that reason, @tt{pair?} evaluates to true on all lists except the empty list.

The empty list has another distinguishing characteristic. It's the only value for which @tt{null?} evaluates to true.

Now it should be clear that @tt{atom?} evaluates to true when the input isn't a list of any sort. Atoms are the things which stand on their own, and at least for the most part evaluate to themselves. Examples include numbers like @tt{5}, strings like @tt{"hello"}, or quoted sequences of characters like @tt{'hello}.

Although it's good to know what lists and atoms are and how they differ, it's also helpful to consider how they're the same. Recall that the @italic{Law of Cons} requires that its second argument is a list, yet it imposes no restrictions on the first argument. It can be a list or an atom, either works. (Yes, this means a list can contains lists.) Rather than having to say "atom or list" all the time, we can refer to these sorts of values as @italic{s-expressions}. @margin-note{While users of object-oriented languages say "everything's an object", Scheme and Lisp programmers say "everything's an s-expression."}

The @tt{eq?} operation answers a yes-or-no question much like the other @tt{?}-suffixed operations, but it accepts two arguments instead of just one. It asks, "are these two non-numeric atom values equal?" A few footnotes indicate that in practice @tt{eq?} may be defined over more types than that. Some quick experiments in the Racket REPL confirm that it seems to work on numeric atoms and lists too.


@section{Do It, Do It Again, and Again, and Again...}

operations: lat? (stands for list-of-atoms?), member?

keywords: define, lambda, cond, else, or

literal values: #t, #f


@section{Cons the Magnificent}

@tt{rember} stands for @italic{remove member}. It takes a value @tt{a} and a list @tt{lst} and returns the same list, except with the first copy of @tt{a} removed. The @tt{cons} operation which combines anything with a list to form a pair (or equivalently, another list) makes it possible to preseve the front of the list while recursively @tt{rember}-ing the rest of the list. Once the base case of a null list is reached, the recursion unwinds and the @tt{cons}es are applied to construct the final list.

@chunk[<rember>
(define rember
  (lambda (a lst)
    (cond ((null? lst) '())
          ((eq? (car lst) a) (cdr lst))
          (else (cons (car lst) (rember a (cdr lst)))))))]

@tt{firsts} takes a list of lists and returns the list consisting of the first element from each inner list. For instance, @tt{(firsts '((a b) (c d)))} evaluates to @tt{'(a c)}. Like all recursive functions on lists, the first thing to check is whether the input list is null. If it is, there is no inner list from which to extract a first element, so the only thing to do is return @tt{'()}. Otherwise, there must be a first element and since the input is supposed to be a list of lists we expect this first element to be a list. The only part of this first inner list we need is the first element, so it's extracted using @tt{caar} (same as @tt{(lambda (x) (car (car x)))}), and then @tt{cons}ed to the @tt{firsts} of the rest of the inner lists. Note that if any of the elements in the input list are @tt{'()}, @tt{caar} will eventually raise an error.

@chunk[<firsts>
(define firsts
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (caar lst)
                      (firsts (cdr lst)))))))]

@tt{seconds} is just like @tt{firsts}, except it extracts the second element from each inner list. In fact, the only difference aside from the name is the use of @tt{cadar} instead of @tt{caar}. Another similarity is unfortunate the lack of robustness: @tt{cadar} will raise an error whenever one of the inner lists has fewer than two elements. Later examples will get more sophisticated and robust.

@chunk[<seconds>
(define seconds
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (cadar lst)
                      (seconds (cdr lst)))))))]

@tt{insertR} scans a list of atoms @tt{lat} for an element equal to @tt{old}. Upon finding a match, @tt{new} is inserted to its right and the resulting list is returned.

@chunk[<insertR>
(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))]

@tt{insertL} is the same as @tt{insertR}, except the @tt{new} value is inserted to the left of the first element that equals @tt{old}. Notice the @tt{eq?} case is much simpler because when @tt{(eq? (car lat) old)} is true, @tt{lat} starts with @tt{old} and all that remains is inserting @tt{new} to the left of it. @tt{insertR} had to extract the @tt{old} element so it could be transposed with the @tt{new} value.

@chunk[<insertL>
(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))]

@tt{subst} is the same as @tt{insertR}/@tt{insertL}, except the first element equal to @tt{old} is removed. The @tt{new} value is @italic{subst}ituted in its place.

@chunk[<subst>
(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new
                                     (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))]

@tt{subst2} resembles @tt{subst}, but it instead takes two old values, @tt{o1} and @tt{o2}, and substitutes the first occurrence of either with @tt{new}. The implementation is almost the same, except @tt{or} is used to check whether the front of the list is equal to either @tt{o1} or @tt{o2}.

@chunk[<subst2>
(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? (car lat) o1)
               (eq? (car lat) o2)) (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst2 new o1 o2 (cdr lat)))))))]

For the most part functions defined in the chapter have been scanning a list until some predicate evaluates to true, at which point the recursive calls stop. For instance, @tt{(rember 'a '(x a a a))} would see that the first element, @tt{x}, is not the same as @tt{a} and recurse with @tt{(cons 'x (rember 'a '(a a a)))}, at which point the first element matches @tt{a}. Only the first match is impacted, so this example evaluates to @tt{'(x a a)}. Although these functions are useful, often the same logic should be applied to the rest of the list, even after finding the first match. @tt{multirember} does just that: it removes every element in the atom list equal to @tt{a}.

@chunk[<multirember>
(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))]

Notice the following @tt{multi}* functions resemble their non-multi analogues, except they recurse even when the predicate is true. Additionally, they all adhere to the Fourth Commandment, passing different and smaller input in every recursive call. Doing otherwise would likely result in infinite loops.

@chunk[<multiinsertR>
(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))]

@chunk[<multiinsertL>
(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))]

@chunk[<multisubst>
(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))]