class Rotary
  include Enumerable
  attr_reader :wheel

  def initialize
    @wheel = [*(0..9)].map(&:to_s)
  end

  def each
    @wheel.each { |c| yield(c) }
  end

  def last
    @wheel.last
  end

  def add1(number)
    begining_numbers, trailing_nines = partition_by(number, '9')
    add_with_carry(begining_numbers) + convert(trailing_nines, '0')
  end

  def sub1(number)
    begining_numbers, trailing_zeros = partition_by(number, '0')
    new_number = sub_with_carry(begining_numbers) + convert(trailing_zeros, '9')
    new_number.empty? ? '0' : new_number
  end

  def recognizes?(n)
    n.chars.all? { |x| @wheel.include? x }
  end

  protected

  def partition_by(number, char)
    number.split(/(#{char}+)$/)
  end

  def add_with_carry(number)
    if number.empty?
      number.insert(0, '1')
    else
      number[-1] = self.next(number[-1])
    end
    number
  end

  def sub_with_carry(number)
    number[-1] = prev(number[-1])
    number.sub(/^0+/, '')
  end

  def convert(trailing_chars, char)
    (trailing_chars ? (char * trailing_chars.size) : '')
  end

  def next(character)
    fail "Unrecognized character #{character}" unless i = find_index(character)
    character == last ? first : @wheel[i].next
  end

  def prev(character)
    fail "Unrecognized character #{character}" unless i = find_index(character)
    character == first ? last : first(i).last
  end
end
