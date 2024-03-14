# echo ./permutations.ex | entr -c elixir /_

ExUnit.start()

defmodule Combinatorics do
  def permutations([]), do: [[]]

  @doc """
  Question: in how many different ways can you arrange x items?

  - for a list of 3 items, there are   `3*2*1 = 3! =  6` permutations (aka arrangements).
  - for a list of 4 items, there are `4*3*2*1 = 4! = 24` permutations.
  """
  @spec permutations(list) :: list
  def permutations(lst) do
    for x <- lst,
        xs <- permutations(lst -- [x]),
        do: [x | xs]
  end

  def subset([]), do: [[]]

  require IEx
  # start iex
  # iex(1)> Code.require_file("./permutations.ex")
  # iex(2)> Combinatorics.subset(['A', 'B', 'C'])

  def subset([x | xs]) do
    left = subset(xs)

    # IEx.pry()
    # right = for ys <- left, do: [x | ys]
    # right = left |> Enum.map(&[x | &1])
    right = left |> Enum.map(fn ys -> [x | ys] end)

    left ++ right
  end
end

defmodule CombinatoricsTest do
  use ExUnit.Case

  test "6 permutations" do
    assert Combinatorics.permutations([1, 2, 3]) == [
             [1, 2, 3],
             [1, 3, 2],
             [2, 1, 3],
             [2, 3, 1],
             [3, 1, 2],
             [3, 2, 1]
           ]
  end

  test "3 combinations" do
    assert Combinatorics.subset(['A', 'B', 'C']) == [
             [],
             ['C'],
             ['B'],
             ['B', 'C'],
             ['A'],
             ['A', 'C'],
             ['A', 'B'],
             ['A', 'B', 'C']
           ]
  end
end
