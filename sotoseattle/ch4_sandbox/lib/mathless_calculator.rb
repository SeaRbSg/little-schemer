class MathlessCalculator
  def initialize
    @rotary = Rotary.new
  end

  def add(n, m)
    return n if zero? m
    @rotary.add1(add(n, @rotary.sub1(m)))
  end

  def sub(n, m)
    return n if zero? m
    @rotary.sub1(sub(n, @rotary.sub1(m)))
  end

  def mul(n, m)
    return '0' if zero? m
    add(n, mul(n, @rotary.sub1(m)))
  end

  def exp(n, m)
    return '1' if zero? m
    mul(n, exp(n, @rotary.sub1(m)))
  end

  private

  def zero?(n)
    n == '0'
  end
end
