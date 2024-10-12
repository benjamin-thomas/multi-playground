/*

echo ./Program.cs | entr -c bash -c 'dotnet run 16 < <(echo -e "ABC\nDEF\nFF")'

 */

namespace Radix;

internal static class Program
{

    private static void Main(string[] args)
    {
        var baseVal = int.Parse(args[0]);
        Console.WriteLine($"Base is: {baseVal}\n---\n");
        RunProgram(baseVal);
    }

    private static void RunProgram(int baseVal)
    {
        while (true)
        {
            var line = Console.ReadLine();

            if (line == null) break;

            // (+ 12 (* 16 (+ 11 (* 16 (+ 10 (* 16 0))))))
            // 2748
            // ---
            // (reduce (fn [acc n] (+ n (* 16 acc))) 0 [10 11 12])
            // 2748
            // ---
            // Prelude> foldl (\acc n -> n + 16*acc) 0 [10, 11, 12]
            // 2748
            Console.WriteLine(line);
            var result =
                line
                    .ToCharArray()
                    .Select(Internal.DigitOfChar)
                    .Aggregate(0, (acc, n) =>
                    {
                        Console.Write($" {n} (acc={acc})\n");
                        return n + baseVal * acc;
                    });

            Console.WriteLine($" -> {result}\n");
        }
    }
}

internal static class Internal
{
    public static int DigitOfChar(char c)
    {
        return c switch
        {
            >= '0' and <= '9' => c - '0',
            >= 'A' and <= 'F' => c - 'A' + 10,
            _ => throw new ArgumentException("Invalid digit: " + c),
        };
    }
}