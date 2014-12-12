require 'spec_helper'


describe 'Rotary' do
  let(:rot) { Rotary.new }

  it 'is a continuum of integers in a range' do
    rango = (3..7)
    r = Rotary.new(rango)
    r.count.must_equal rango.size
    r.first.must_equal ?3
    r.last.must_equal ?7
  end

  it 'by default knows only the 10 digital caracters' do
    %w(0 1 2 3 4 5 6 7 8 9).all? { |c| rot.wheel.must_include(c) }
  end

  it 'does not understand anything but the defined characters' do
    proc { rot.next(2) }.must_raise RuntimeError
    proc { rot.next('a') }.must_raise RuntimeError
  end

  it 'only knows about the next and previous characters' do
    rot.must_respond_to(:next)
    rot.must_respond_to(:prev)
  end

  it 'given a character it tells you what the next character is' do
    rot.next('2').must_equal '3'
  end

  it 'given a character it tells you what the previous character is' do
    rot.prev('6').must_equal '5'
  end

  it 'as a ring, the next of the last is the first' do
    rot.next('9').must_equal '0'
  end

  it 'as a ring, the previous of the first is the last' do
    rot.prev('0').must_equal '9'
  end
end
