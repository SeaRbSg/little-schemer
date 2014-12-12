require 'spec_helper'


describe 'Rotary' do
  let(:rot) { Rotary.new }

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

  describe '#add1 : add 1 to an integer in str form' do
    it { rot.add1('1').must_equal '2' }
    it { rot.add1('0').must_equal '1' }
    it { rot.add1('10').must_equal '11' }
    it { rot.add1('1999').must_equal '2000' }
    it { rot.add1('9').must_equal '10' }
    it { rot.add1('999').must_equal '1000' }
  end

  describe '#sub1 : subtract 1 to an integer in str form' do
    it { rot.sub1('2').must_equal '1' }
    it { proc { rot.sub1('0') }.must_raise RuntimeError }
    it { rot.sub1('11').must_equal '10' }
    it { rot.sub1('10').must_equal '9' }
    it { rot.sub1('100').must_equal '99' }
    it { rot.sub1('321').must_equal '320' }
    it { rot.sub1('1').must_equal '0' }
  end
end
