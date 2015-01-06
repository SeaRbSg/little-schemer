p. 151
------

A partial function is one that doesn't map all of its domain to an output.
For example, a square root function on the integers

    g :: Int -> Int
    g x = sqrt x

is undefined for negative integers, and therefore is a partial function.

The equivalent total function would be

    g :: Int -> Complex
    g x = sqrt x


***

Why is _looking_ a partial function? It fulfulls that criteria if 
not-terminating counts as "undefined"

p. 154
------

"Is _align\*_ a partial function?"

I don't really understand the proof for this...

