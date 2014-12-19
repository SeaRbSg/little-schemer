require 'spec_helper'

describe 'MathlessCalculator' do
  let(:calc) { MathlessCalculator.new }

  it { calc.compute('(2 + 2)').must_equal '4' }
  it { calc.compute('(2 + (1 + 1))').must_equal '4' }
  it { calc.compute('(((1 + 1) + (1 + 1)) + (1 + 1))').must_equal '6' }

  it { calc.compute('(3 x 2)').must_equal '6' }
  it { calc.compute('((1 x (1 x (1 x (1 x (1 x 1))))) x 1)').must_equal '1' }
  it { calc.compute('((1 x (1 x (1 x (1 x (1 x 1))))) x 0)').must_equal '0' }
  it { calc.compute('((2 x (1 x (1 x (1 x (1 x 1))))) x 1)').must_equal '2' }

  it { calc.compute('(2 ^ 3)').must_equal '8' }
  it { calc.compute('(1 ^ 10)').must_equal '1' }
  it { calc.compute('(1 ^ 0)').must_equal '1' }
  # it { calc.compute('(2 ^ 9)').must_equal '512' } # a bit slow
  it { calc.compute('((1 + 1) ^ 3)').must_equal '8' }
  it { calc.compute('((2 x 2) ^ (1 + 1))').must_equal '16' }

  it { calc.compute('(3 - 2)').must_equal '1' }
  it { calc.compute('((4 - 1) - (3 - 1))').must_equal '1' }
  it { calc.compute('(((2 ^ 2) - (1 x 1)) - (3 - (0 + 1)))').must_equal '1' }
end
