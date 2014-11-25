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