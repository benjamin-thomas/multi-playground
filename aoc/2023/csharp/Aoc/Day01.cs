using System.Runtime.InteropServices;

namespace Aoc
{
    public static class Day01
    {

        public static void Run()
        {
            List<String> lines = [];

            while (true)
            {
                String? line = Console.ReadLine();
                if (line == null) break;
                lines.Add(line);
            }

            int part1 = ProcessLines1(lines);
            int part2 = ProcessLines2(lines);
            Console.WriteLine($"Part 1: {part1}");
            Console.WriteLine($"Part 2: {part2}");

        }

        public static List<int> FilterMap1(String str)
        {
            List<int> list = [];

            foreach (Char c in str)
                if (Char.IsDigit(c))
                    list.Add(c - '0');

            return list;
        }


        public static List<int> FilterMap1Func(string str)
        {
            return str
                .Where(Char.IsDigit)     // filter
                .Select(c => c - '0')    // map
                .ToList();
        }

        public static List<int> FilterMap2(String str)
        {
            List<int> list = [];

            for (int i = 0; i < str.Length; i++)
            {
                Char c = str[i];
                String frag = str[i..];

                Func<String, Boolean> startsWith = frag.StartsWith;

                if (Char.IsDigit(c))
                    list.Add(c - '0');
                else if (startsWith("one"))
                    list.Add(1);
                else if (startsWith("two"))
                    list.Add(2);
                else if (startsWith("three"))
                    list.Add(3);
                else if (startsWith("four"))
                    list.Add(4);
                else if (startsWith("five"))
                    list.Add(5);
                else if (startsWith("six"))
                    list.Add(6);
                else if (startsWith("seven"))
                    list.Add(7);
                else if (startsWith("eight"))
                    list.Add(8);
                else if (startsWith("nine"))
                    list.Add(9);
            }
            return list;
        }

        public static List<int> FilterMap2Func(string str)
        {
            int ToInt(char c, int i)
            {
                String frag = str[i..];
                if (Char.IsDigit(c))
                    return c - '0';
                else if (frag.StartsWith("one"))
                    return 1;
                else if (frag.StartsWith("two"))
                    return 2;
                else if (frag.StartsWith("three"))
                    return 3;
                else if (frag.StartsWith("four"))
                    return 4;
                else if (frag.StartsWith("five"))
                    return 5;
                else if (frag.StartsWith("six"))
                    return 6;
                else if (frag.StartsWith("seven"))
                    return 7;
                else if (frag.StartsWith("eight"))
                    return 8;
                else if (frag.StartsWith("nine"))
                    return 9;
                else return -1;
            }

            return str
                .Select(ToInt)
                .Where(x => x != -1)
                .ToList();
        }

        private static int ProcessLines(List<String> lines, Func<String, List<int>> filterMap)
        {
            int sum = 0;
            foreach (var line in lines)
            {
                List<int> nums = filterMap(line);
                int lineTotal = 0;
                lineTotal += nums.First() * 10;
                lineTotal += nums.Last();

                sum += lineTotal;
            }
            return sum;
        }

        private static int ProcessLines1(List<String> lines)
        {
            return ProcessLines(lines, FilterMap1);
        }

        private static int ProcessLines2(List<String> lines)
        {
            return ProcessLines(lines, FilterMap2);
        }

    }
}