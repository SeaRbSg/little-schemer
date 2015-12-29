# Glossary

As we figure out some of the bad names, put the definitions here:

## Little Schemer

Builtins | Definiton
---------|----------
cons     | _cons_ truct a pair (aka `cons-cell`)
car      | first element of a pair
cdr      | rest of the pair

Ch2     | Definiton
--------|----------
lat?    | 'List of Atoms?' (instead of, say, a list of lists)
member? | Does this atom exist in this list?

Ch3         | Definiton
------------|----------
rember      | 'Remove member' – remove first occurance of an atom in a list
multirember | 'Multiple Remove Member' – removes all matching items from the list
subst       | 'Substitute' – replace first occurance of an atom in a list with the given atom

Ch4     | Definiton
--------|----------
tup 	| 'Tuple' – a list of numbers
pick    | get nth element of list
rempick | remove nth element from list
eqan?   | 'Equal atom-or-number?' – are the two given atoms the same?

Ch5     | Definiton
--------|----------
*       | indicates function works on nested lists (aka any S-expr, not just lists of atoms)
rember* | remove member from nested list
eqlist? | Do these two lists have the same structure and same atoms?
equal?  | general S-expr equality. Checks for eqan? and eqlist?

Ch6  | Definiton
-----|----------
aexp | 'Arithmetic expression' – a list of numbers and mathy symbols

Note: If you figure out what the authors mean by 'shadows', please tell me! -L

Ch7      | Definiton
---------|----------
rel      | 'Relation' – A not-neccesarily-unique mapping.
fun      | 'Function' – (functions are just really big hashes! hashes are just tiny functions!)
fun?     | do inputs map to a unique output?
revrel   | 'Reverse relation'
fullfun? | 'Full function?' - Checks that no two inputs map to the same output. aka a 'one-to-one' function, aka an 'injection'

Ch8            | Definiton
---------------|----------
rember-f       | a function that takes a test for equality and returns a function that will remove the first item of a list that passes that test
seqL           | takes two items and prepends them to a list
seqR           | takes two items and prepends them *backward* to a list
multirember-f  | a function that takes a test for equality and returns a function that will remove all items of a list that pass that test
multirember-T  | takes a boolean function and a list, and removes all elements that return true (aka `reject` in Ruby, also called a 'filter' in functional languages) Note: I have no idea what the `T` stands for here!
&co            | Give a function that's applied at the very end.  Multirember&co, for instance, applies multirember to a list to get two lists: the list without the member, and the removed items. It then calls the `&co` with the two lists.  A function that's called at the very end of a computation is called a 'continuation'
multirember&co | remove multiple members and call continuation
a-friend, new-friend, latest-friend, last-friend | Arbitrary functions. It's important that _they are functions_, and that they take two lists as arguments, but what they do is pretty meaningless (like, 'return true if the second list is empty!')

Ch9  | Definiton
-----|----------
sorn | 'Symbol or Number'
pora | 'Pair or Atom'

Ch10  | Definiton
------|----------
table | Acts like a list of hashes. It's used here for storing and looking up variables.

## Seasoned

## Reasoned

Name  | Definiton
------| ---------
sal   | 'Succeeds At Least once'. Man, these names are the *worst*
