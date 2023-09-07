# frozen_string_literal: true

require 'minitest/autorun'

require './test/config'
require './lib/problems'

# rg --files | COLOR=1 entr -c ruby -Itest ./test/problems_test.rb

class ProblemsTest < Minitest::Test
  module P
    extend Problems
  end

  def test_last
    assert_nil P.last([])
    assert_equal 1, P.last([1])
    assert_equal 2, P.last([1, 2])
    assert_equal 3, P.last([1, 2, 3])

=begin
  I overflow the stack with about 8800 items.

  The VM can be initialized with a TCO flag (see: ./test/config.rb), but computation becomes extremely slow when going
  over a small threshold anyways (4s for 100k items).
=end
    range = 0..8700
    assert_equal range.last, P.last(range.to_a)
  end
end
