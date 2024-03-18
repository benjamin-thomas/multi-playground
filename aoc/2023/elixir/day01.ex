# echo ./day01.ex | entr -c elixir /_

defmodule NumbersSimple do
  def filter_map(str) do
    filter_map(str, [])
  end

  defp filter_map(<<>>, acc), do: Enum.reverse(acc)

  defp filter_map(<<n, rest::binary>>, acc) when n in ?0..?9 do
    filter_map(rest, [n - ?0 | acc])
  end

  defp filter_map(<<_, rest::binary>>, acc) do
    filter_map(rest, acc)
  end
end

defmodule Numbers do
  def filter_map(str) do
    filter_map(str, [])
  end

  defp filter_map(<<>>, acc), do: Enum.reverse(acc)

  defp filter_map(<<n, rest::binary>>, acc) when n in ?0..?9 do
    filter_map(rest, [n - ?0 | acc])
  end

  defp filter_map(<<?o, ?n, ?e, rest::binary>>, acc) do
    filter_map(<<?n, ?e, rest::binary>>, [1 | acc])
  end

  defp filter_map(<<?t, ?w, ?o, rest::binary>>, acc) do
    filter_map(<<?w, ?o, rest::binary>>, [2 | acc])
  end

  defp filter_map(<<?t, ?h, ?r, ?e, ?e, rest::binary>>, acc) do
    filter_map(<<?h, ?r, ?e, ?e, rest::binary>>, [3 | acc])
  end

  defp filter_map(<<?f, ?o, ?u, ?r, rest::binary>>, acc) do
    filter_map(<<?o, ?u, ?r, rest::binary>>, [4 | acc])
  end

  defp filter_map(<<?f, ?i, ?v, ?e, rest::binary>>, acc) do
    filter_map(<<?i, ?v, ?e, rest::binary>>, [5 | acc])
  end

  defp filter_map(<<?s, ?i, ?x, rest::binary>>, acc) do
    filter_map(<<?i, ?x, rest::binary>>, [6 | acc])
  end

  defp filter_map(<<?s, ?e, ?v, ?e, ?n, rest::binary>>, acc) do
    filter_map(<<?e, ?v, ?e, ?n, rest::binary>>, [7 | acc])
  end

  defp filter_map(<<?e, ?i, ?g, ?h, ?t, rest::binary>>, acc) do
    filter_map(<<?i, ?g, ?h, ?t, rest::binary>>, [8 | acc])
  end

  defp filter_map(<<?n, ?i, ?n, ?e, rest::binary>>, acc) do
    filter_map(<<?i, ?n, ?e, rest::binary>>, [9 | acc])
  end

  defp filter_map(<<_, rest::binary>>, acc) do
    filter_map(rest, acc)
  end
end

defmodule Day01 do
  def read_input do
    File.read!("../_inputs/day01.txt")
  end

  def extract_digits(str) do
    # Part 1: 55002
    NumbersSimple.filter_map(str)
  end

  def extract_more_digits(str) do
    # Part 2: 55093
    Numbers.filter_map(str)
  end

  def keep_outer_or_zeros(lst) do
    case lst do
      [] ->
        {0, 0}

      [x] ->
        {x, x}

      [x, y] ->
        {x, y}

      [x | xs] ->
        Enum.reduce(xs, fn y, _ -> {x, y} end)
    end
  end

  def process_lines(str, extract_fn) do
    str
    |> String.split("\n", trim: true)
    |> Enum.map(extract_fn)
    |> Enum.map(&keep_outer_or_zeros/1)
    |> Enum.map(fn {x, y} -> x * 10 + y end)
    |> Enum.sum()
  end

  def part1(str) do
    process_lines(str, &extract_digits/1)
  end

  def part2(str) do
    process_lines(str, &extract_more_digits/1)
  end
end

Day01.read_input() |> Day01.part1() |> IO.inspect(label: "Part 1")
Day01.read_input() |> Day01.part2() |> IO.inspect(label: "Part 2")

ExUnit.start()

defmodule Day01Test do
  use ExUnit.Case, async: true

  test "extract_digits" do
    assert Day01.extract_digits("1abc2") == [1, 2]
    assert Day01.extract_digits("pqr3stu8vwx") == [3, 8]
    assert Day01.extract_digits("a1b2c3d4e5f") == [1, 2, 3, 4, 5]
  end

  test "keep_outer_or_zeros" do
    assert Day01.keep_outer_or_zeros([]) == {0, 0}
    assert Day01.keep_outer_or_zeros([1]) == {1, 1}
    assert Day01.keep_outer_or_zeros([1, 2]) == {1, 2}
    assert Day01.keep_outer_or_zeros([1, 2, 3]) == {1, 3}
  end

  test "extract_more_digits" do
    assert Numbers.filter_map("1abc23xyz.four.nineight") == [1, 2, 3, 4, 9, 8]
  end
end

defmodule NumbersTest do
  use ExUnit.Case

  test "extract_more_digits" do
    assert Numbers.filter_map("1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789") ==
             [
               1,
               1,
               8,
               9,
               8,
               7,
               9,
               6,
               5,
               4,
               3,
               2,
               1,
               0,
               1,
               2,
               3,
               4,
               5,
               6,
               7,
               8,
               9
             ]
  end
end
