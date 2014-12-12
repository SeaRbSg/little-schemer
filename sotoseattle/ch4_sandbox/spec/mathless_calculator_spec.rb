require 'spec_helper'

describe 'MathlessCalculator' do
  let(:calc) { MathlessCalculator.new }

  describe '#add : add two integer together' do
    it { calc.add('2', '2').must_equal '4' }
    it { calc.add('0', '0').must_equal '0' }
    it { calc.add('11', '9').must_equal '20' }
  end

  describe '#sub : subtract two integers' do
    it { calc.sub('2', '0').must_equal '2' }
    it { calc.sub('2', '2').must_equal '0' }
    it { calc.sub('20', '2').must_equal '18' }
    it { proc { calc.sub('2', '4') }.must_raise RuntimeError }
  end

  describe '#mul : multiply two integers' do
    it { calc.mul('2', '1').must_equal '2' }
    it { calc.mul('2', '2').must_equal '4' }
    it { calc.mul('20', '2').must_equal '40' }
  end

  describe '#exp : exponentiation of two integers' do
    it { calc.exp('2', '1').must_equal '2' }
    it { calc.exp('2', '3').must_equal '8' }
    it { calc.exp('1', '20').must_equal '1' }
  end
end
