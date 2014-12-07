require 'spec_helper'

describe 'NoNumberCalculator' do
  describe '#add1 : add 1 to an integer in str form' do
    it { add1('1').must_equal '2' }
    it { add1('0').must_equal '1' }
    it { add1('10').must_equal '11' }
    it { add1('1999').must_equal '2000' }
    it { add1('9').must_equal '10' }
    it { add1('999').must_equal '1000' }
  end

  describe '#sub1 : subtract 1 to an integer in str form' do
    it { sub1('2').must_equal '1' }
    it { proc { sub1('0') }.must_raise RuntimeError }
    it { sub1('11').must_equal '10' }
    it { sub1('10').must_equal '9' }
    it { sub1('100').must_equal '99' }
    it { sub1('321').must_equal '320' }
    it { sub1('1').must_equal '0' }
  end

  describe '#add : add two integer together' do
    it { add('2', '2').must_equal '4' }
    it { add('0', '0').must_equal '0' }
    it { add('11', '9').must_equal '20' }
  end

  describe '#sub : subtract two integers' do
    it { sub('2', '0').must_equal '2' }
    it { sub('2', '2').must_equal '0' }
    it { sub('20', '2').must_equal '18' }
    it { proc { sub('2', '4') }.must_raise RuntimeError }
  end

  describe '#mul : multiply two integers' do
    it { mul('2', '1').must_equal '2' }
    it { mul('2', '2').must_equal '4' }
    it { mul('20', '2').must_equal '40' }
  end

  describe '#exp : exponentiation of two integers' do
    it { exp('2', '1').must_equal '2' }
    it { exp('2', '3').must_equal '8' }
    it { exp('1', '20').must_equal '1' }
  end
end
