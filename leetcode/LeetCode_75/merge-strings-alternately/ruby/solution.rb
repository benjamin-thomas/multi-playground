# echo ./solution.rb | entr -c ruby /_

def pad(arr, final_len)
  pad_len = final_len - arr.length
  if pad_len > 0 then
    arr.concat([nil] * pad_len)
  else
    arr
  end
end

def interleaved(word1, word2)
  largest = [word1.length, word2.length].max
  chars1 = pad(word1.chars, largest)
  chars2 = pad(word2.chars, largest)

  chars1.zip(chars2)
    .flatten
    .compact
    .join
end


require 'minitest/autorun'
class TestSolution < Minitest::Test

  def test_padding
   assert_equal([1,2,3,nil,nil], pad([1,2,3], 5))
   assert_equal([1,2,3], pad([1,2,3], 3))
   assert_equal([1,2,3], pad([1,2,3], 0))
   assert_equal([], pad([], 0))
   assert_equal([nil,nil,nil], pad([], 3))
  end

  def test_solution
    assert_equal("ABCDEF", interleaved("AC", "BDEF"))
    assert_equal("ABCDEF", interleaved("ACEF", "BD"))
    assert_equal("ABCD", interleaved("AC", "BD"))
    assert_equal("", interleaved("", ""))
  end

end
