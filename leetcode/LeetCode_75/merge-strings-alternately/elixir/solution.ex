# echo ./solution.ex | entr -c elixir /_

ExUnit.start()

defmodule Solution do
  def interleave_letters("", ""), do: ""
  def interleave_letters(<<x::binary-size(1)>> <> xs, ""), do: x <> interleave_letters(xs, "")
  def interleave_letters("", <<y::binary-size(1)>> <> ys), do: y <> interleave_letters("", ys)

  def interleave_letters(<<x::binary-size(1)>> <> xs, <<y::binary-size(1)>> <> ys) do
    x <> y <> interleave_letters(xs, ys)
  end
end

defmodule Test do
  use ExUnit.Case, async: true

  test "interleave_letters" do
    assert "ABCD" == Solution.interleave_letters("AC", "BD")
    assert "ABCD_EF" == Solution.interleave_letters("AC", "BD_EF")
    assert "ABCD_EF" == Solution.interleave_letters("AC_EF", "BD")
    assert "ABC" == Solution.interleave_letters("ABC", "")
    assert "ABC" == Solution.interleave_letters("", "ABC")
    assert "" == Solution.interleave_letters("", "")
  end
end
