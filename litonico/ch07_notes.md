Ch. 7 Notes
-----------

p.120

There are a bunch of conceptual leaps here! 

The first is using a list of pairs to represent a dictionary 
(a.k.a Ruby's **Hash**):

    '((8 3) (4 2) (7 6) (6 2) (3 4)) ;; confusingly not quoted in the text

    is the structure Little Schemer uses to notate

    {
      8 => 3,
      4 => 2, 
      7 => 6,
      6 => 2,
      3 => 4
    }

The second conceptual leap is using a dictionary to represent a _function_,
which is why the function name is _fun?_.

A function is just a mapping from inputs to outputs. So is a dictionary.
Dictionaries and functions are the same thing. Isn't that strange?

The difference is that, for numbers, dictionaries can't have a truly 
exhaustive list of mappings (there are infinietly many integers, after all!) 
So dictionaries are _partial_ functions, where 'regular' functions 
are called _total_-- at least for integers.

Both dictionaries and functions have to have a unique set of mappings, too--
if I look up `my_dictionary[3]`, I can't expect it to be both `4` and `5`.
Likewise, if I try to write a function `f(x)` where `f(3) -> 4` and 
`f(3) -> 5`, it's... not a function.

This is why Little Schemer checks `(set? (firsts l))`. If the inputs are not
a set (i.e. there are duplicates), then a single input maps to multiple outputs,
and it's not a function. Or a dictionary. Whichever.

p.121
A _fullfun_ is an injective function.
