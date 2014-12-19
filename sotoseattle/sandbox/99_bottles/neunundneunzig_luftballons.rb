# why not the european version of 99 bottles of beer?
# 99 luftballons by Nena, at least the title seems similar
class NeunundneunzigLuftbaloons
  def initialize(max)
    @max = max
  end

  def sing!
    [*(1..@max)].reverse_each.map { |n| drunken_chorus(n) }.join("\n") << fuga
  end

  private

  def drunken_chorus(n)
    opening_verse(n) + "\n" + closing_verse(n - 1)
  end

  def opening_verse(n)
    "#{pluralize(n)} floating about, #{pluralize(n)}, yea!."
  end

  def closing_verse(n)
    "Take one down and pass it around, #{pluralize(n)} floating aboooouut."
  end

  def fuga
    "\nGo to the store and buy some more, #{@max} luft_baloons floating about. (Badaboom!)"
  end

  def pluralize(n)
    return 'no more luft_baloons' if n == 0
    n == 1 ? '1 luft_baloon' : "#{n} luft_baloons"
  end
end

# nena = NeunundneunzigLuftbaloons.new(5)
# puts nena.sing!
