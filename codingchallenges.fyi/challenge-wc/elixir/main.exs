# rg --files | entr -c bash -c 'time elixir ./main.exs ../test.txt'

defmodule Main do
  defp count_stats(line, {bytes, lines, words, runes}) do
    {bytes + byte_size(line), lines + 1, words + length(String.split(line)),
     runes + String.length(line)}
  end

  defp handle_path(path) do
    counters =
      path
      |> File.stream!()
      |> Enum.reduce({0, 0, 0, 0}, &count_stats/2)

    IO.inspect(path: path, counters: counters)
  end

  def start() do
    System.argv()
    |> Enum.each(&handle_path/1)
  end
end

Main.start()
