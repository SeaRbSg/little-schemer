# hi_STeRI_CALc: String calculator based on Little Schemer Chpt 4
# A calculator for strings. No number operations allowed (almost).
# How far can we get this?

module NoNumberCalculator
  INT_RING = %w(0 1 2 3 4 5 6 7 8 9)

  def add(n, m)
    return n if zero? m
    add1(add(n, sub1(m)))
  end

  def sub(n, m)
    return n if zero? m
    sub1(sub(n, sub1(m)))
  end

  def mul(n, m)
    return '0' if zero? m
    add(n, mul(n, sub1(m)))
  end

  def exp(n, m)
    return '1' if zero? m
    mul(n, exp(n, sub1(m)))
  end

  private

  def zero?(n)
    n == '0'
  end

  def add1(n)
    carry_over = true
    sol = n.chars.reverse.map do |c|
      if carry_over
        c = get_next_int(c)
        carry_over = false if c != '0'
      end
      c
    end.reverse.join
    sol.insert(0, '1') if carry_over
    sol
  end

  def sub1(n)
    carry_over = true
    sol = n.chars.reverse.map do |c|
      if carry_over
        c = get_prev_int(c)
        carry_over = false if c != '9'
      end
      c
    end
    fail RuntimeError if carry_over
    sol = sol.reverse.join.sub(/^0+/, '')
    sol.empty? ? sol = '0' : sol
  end

  def get_next_int(c)
    i = INT_RING.index(c)
    i = -1 if i == 9
    INT_RING[i + 1]
  end

  def get_prev_int(c)
    i = INT_RING.index(c)
    i = 10 if i == 0
    INT_RING[i - 1]
  end
end
