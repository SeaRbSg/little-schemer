class Rotary
  include Enumerable
  attr_reader :wheel

  def initialize(range = (0..9))
    @wheel = range.to_a.map(&:to_s)
  end

  def each
    @wheel.each { |c| yield(c) }
  end

  def next(character)
    fail 'Unrecognized character' unless i = find_index(character)
    character == last ? first : @wheel[i].next
  end

  def prev(character)
    fail 'Unrecognized character' unless i = find_index(character)
    character == first ? last : first(i).last
  end

  def last
    @wheel.last
  end
end
