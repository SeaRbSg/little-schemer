MATHLESS CALCULATOR for the MASSES
==================================

<img src="./public/power_to_the_mathless.png" align="right"/>

### Why?

If you are like me, a sensitive personality grounded in the world of Humanities, you cannot but cringe at all those geeky math equations so in vogue these days (statistics, differential equations, machine learning libraries, game theory gooey, etc.) Enough is enough! Join me in the fight to take our codebase away from the 1% (the math elites) so the 99% can enjoy the fruits of technological progress!

### Whatf??

This is a simple math calculator for the rest of us, the mathematically challenged masses of the world. Instead of working with dry numbers we like to operate with the rich and expressive English characters. Furthermore, not a single mathematical operation is allowed in the code. No. Math. Stop. So no need to hide your hand under the desk to keep count of how many digits you carry when multiplying.

### How?

The building block is the Rotary-Continuum-Grammar-Device (Rotary for short), an array of characters in logical succession that knows how to find the next and the previous character for any given one. Not a single number (Integers and such) is used nor harmed when operating our Mathless Calculator for the Masses.

```ruby
  (0..9) #=> ['0', '1', '2', '3', '4',..., '9']
  next('2') #=> '3'
  next('9') #=> '0'
  prev('0') #=> '9'
  prev('2') #=> '1'
```

Armed only with those two operations we can define for any string based number the addition and subtraction by 1.

Then, it only takes a small leap of faith to create complex mathematically operations with recursion. With the power of recursion we can do things like adding and subtracting numbers, but everything done mathematica-less-ly and in style.

### Usage

```ruby
calc = MathlessCalculator.new
calc.compute('(((1 + 1) + (1 + 1)) + (1 + 1))') # => '6'
calc.compute('(((2 ^ 2) - (1 x 1)) - (3 - (0 + 1)))') # => '1'
```

### Disclaimer:

- It is not a very efficient calculator, slow and quite limited. Enjoy its parsimony and revere its convoluted way of operating.
- The input parenthetical expression has to be well formed, the parsing is quite brittle.
