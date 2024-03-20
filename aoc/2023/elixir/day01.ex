# echo ./day01.ex | entr -c elixir /_

defmodule ExtractNumbersSimple do
  defp filter_map(<<>>, acc), do: Enum.reverse(acc)

  defp filter_map(<<n, rest::binary>>, acc) when n in ?0..?9 do
    filter_map(rest, [n - ?0 | acc])
  end

  defp filter_map(<<_, rest::binary>>, acc) do
    filter_map(rest, acc)
  end

  def filter_map(str) do
    filter_map(str, [])
  end
end

defmodule ExtractNumbersComplex do
  defp filter_map(str, acc) do
    case str do
      <<>> ->
        Enum.reverse(acc)

      <<n, rest::binary>> when n in ?0..?9 ->
        filter_map(rest, [n - ?0 | acc])

      <<?o, ?n, ?e, rest::binary>> ->
        filter_map(<<?e, rest::binary>>, [1 | acc])

      <<?t, ?w, ?o, rest::binary>> ->
        filter_map(<<?o, rest::binary>>, [2 | acc])

      <<?t, ?h, ?r, ?e, ?e, rest::binary>> ->
        filter_map(<<?e, rest::binary>>, [3 | acc])

      <<?f, ?o, ?u, ?r, rest::binary>> ->
        filter_map(<<?r, rest::binary>>, [4 | acc])

      <<?f, ?i, ?v, ?e, rest::binary>> ->
        filter_map(<<?e, rest::binary>>, [5 | acc])

      <<?s, ?i, ?x, rest::binary>> ->
        filter_map(<<?x, rest::binary>>, [6 | acc])

      <<?s, ?e, ?v, ?e, ?n, rest::binary>> ->
        filter_map(<<?n, rest::binary>>, [7 | acc])

      <<?e, ?i, ?g, ?h, ?t, rest::binary>> ->
        filter_map(<<?t, rest::binary>>, [8 | acc])

      <<?n, ?i, ?n, ?e, rest::binary>> ->
        filter_map(<<?e, rest::binary>>, [9 | acc])

      <<_, rest::binary>> ->
        filter_map(rest, acc)
    end
  end

  def filter_map(str) do
    filter_map(str, [])
  end
end

defmodule Day01 do
  def read_input do
    File.read!("../_inputs/day01.txt")
  end

  def extract_digits(str) do
    # Part 1: 55002
    ExtractNumbersSimple.filter_map(str)
  end

  def extract_more_digits(str) do
    # Part 2: 55093
    ExtractNumbersComplex.filter_map(str)
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

ExUnit.start(autorun: false)

defmodule Day01Test do
  use ExUnit.Case, async: false

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
    assert ExtractNumbersComplex.filter_map("1abc23xyz.four.nineight") == [1, 2, 3, 4, 9, 8]
  end
end

defmodule ExtractNumbersComplexTest do
  use ExUnit.Case, async: true

  test "extract_more_digits" do
    assert ExtractNumbersComplex.filter_map(
             "1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789"
           ) ==
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

ExUnit.run()

IO.puts("=== Answers ===")
Day01.read_input() |> Day01.part1() |> IO.inspect(label: "Part 1")
Day01.read_input() |> Day01.part2() |> IO.inspect(label: "Part 2")
