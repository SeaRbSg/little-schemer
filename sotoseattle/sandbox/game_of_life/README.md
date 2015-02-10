### Conway's Game of Life (according to Javier Soto)

#### In Scheme

- Coded in Dr Racket (Racket lang)
- Purely functional and stateless
- 100% recursive
- Not very well tested, I am afraid
- No visualization (yet, Ryan!! help!!!)
- Needs heavy refactoring, because it was coded in a single pass

To run open file in Dr. Racket and run with:

```scheme
(tick 10 '((5 5) (5 6) (5 7)))
```

The procedure 'tick' takes two arguments: the number of recursions and a list of living cells (seed).

#### Next Steps

- Visualization would be nice.
- Man, refactor, it is embarrasing!

