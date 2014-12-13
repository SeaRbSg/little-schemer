MATHLESS CALCULATOR
===================

<img src="./public/power_to_the_mathless.png" align="right"/>

### Why?

If you are like me, a sensitive personality grounded in the world of Humanities, you cannot but cringe at all those geeky math equations so in vogue these days (statistics, differential equations, machine learning libraries, game theory gooey, etc.) Enough is enough! Join me in the fight to take our codebase away from the 1% (the math elites) so the 99% can enjoy the fruits of technological progress!

### Whatf??


This is a simple math calculator for the rest of us, the mathematically challenged masses of the world. Instead of working with dry numbers we like to operate with the rich and expressive English characters, and not a single math operation is allowed in the code. No math.

### How?

The building block is the Rotary-Continuum-Grammar-Device (Rotary for short), an array of characters in logical succession that knows how to find the next and the previous character for any given one.

```ruby
  (0..9) #=> ['0', '1', '2', '3', '4',..., '9']
  next('2') #=> '3'
  next('9') #=> '0'
  prev('0') #=> '9'
  prev('2') #=> '1'
```

Armed only with those two operations we can define for any string based number the addition and subtraction by 1.

Then, it is only a small leap of faith to create complex mathematically operation with recursion. The  same as adding and subtracting numbers, but we done mathematica-less-ly and in style.

```ruby
calc = MathlessCalculator.new
calc.add('40', '2') #=> '42'
calc.sub('10', '1') #=>  '9'
calc.mul('5', '2')  #=> '10'
calc.exp('2', '3')  #=>  '8'
```
