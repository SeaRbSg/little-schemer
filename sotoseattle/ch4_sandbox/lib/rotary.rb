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

  def add1(number_string)
    begining_numbers, trailing_nines = partition_by(number_string, '9')
    add_with_carry(begining_numbers) + convert(trailing_nines, '0')
  end

  def sub1(number_string)
    begining_numbers, trailing_zeros = partition_by(number_string, '0')
    new_number = sub_with_carry(begining_numbers) + convert(trailing_zeros, '9')
    new_number.empty? ? '0' : new_number
  end

  protected

  def partition_by(number_string, char)
    number_string.split(/(#{char}+)$/)
  end

  def add_with_carry(number_string)
    if number_string.empty?
      number_string.insert(0, '1')
    else
      number_string[-1] = self.next(number_string[-1])
    end
    number_string
  end

  def sub_with_carry(number_string)
    number_string[-1] = prev(number_string[-1])
    number_string.sub(/^0+/, '')
  end

  def convert(trailing_chars, char)
    (trailing_chars ? (char * trailing_chars.size) : '')
  end

  def next(character)
    fail 'Unrecognized character' unless i = find_index(character)
    character == last ? first : @wheel[i].next
  end

  def prev(character)
    fail 'Unrecognized character' unless i = find_index(character)
    character == first ? last : first(i).last
  end
end
