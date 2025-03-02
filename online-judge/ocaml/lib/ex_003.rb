def solution str
  (int, dec) = str.gsub(/^0+/, '').split('.')
  return str if int.empty?

  new_int = int.reverse.chars.each_slice(3).map(&:join).join(",").reverse
  if dec.nil?
    new_int
  else
    new_int + "." + dec
  end
end


=begin
echo ./lib/ex_003.rb | entr -c ruby -r minitest/autorun /_
=end
class Test < Minitest::Test
  def test_1
    assert_equal "123", solution("123")
    assert_equal "123.99", solution("123.99")
    assert_equal "1,234", solution("1234")
    assert_equal "12,345", solution("12345")
    assert_equal "123,456", solution("123456")
    assert_equal "1,234,567", solution("1234567")
    assert_equal "1,234,567.88", solution("0001234567.88")
    assert_equal  "0.123", solution("0.123")
  end
end
