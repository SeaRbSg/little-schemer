require 'parser'
require 'rotary'

class MathlessCalculator
  include Parser

  def initialize
    @rotary = Rotary.new
  end

  def compute(number_exp)
    tally(*Parser::arrafy(number_exp))
  end

  protected

  def tally(number_exp)
    return number_exp if number?(number_exp)
    n1, operation, n2 = number_exp
    fail "Kaboom! #{number_exp}" unless n2
    self.send(:"#{operation}", tally(n1), tally(n2))
  end

  def +(n, m)
    return n if zero? m
    @rotary.add1(self.+(n, @rotary.sub1(m)))
  end

  def -(n, m)
    return n if zero? m
    @rotary.sub1(self.-(n, @rotary.sub1(m)))
  end

  def x(n, m)
    return '0' if zero? m
    self.+(n, self.x(n, @rotary.sub1(m)))
  end

  def ^(n, m)
    return '1' if zero? m
    self.x(n, self.^(n, @rotary.sub1(m)))
  end

  def number?(n)
    return false unless n.is_a? String
    @rotary.recognizes? n
  end

  def zero?(n)
    n == '0'
  end
end
