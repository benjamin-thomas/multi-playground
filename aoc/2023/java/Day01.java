/*

SIMPLE SETUP:

echo ./Day01.java | entr -c java -ea /_

-ea: stands for enable assertion

---

LESS SIMPLE SETUP:

find -name '*.java' | entr -c bash -c './manage/build && ./manage/test'

Also, in vscode, change the class path configuration by adding the jars in lib
to the 'Referenced Libraries' section.

---

$ ./manage/run < ../_inputs/day01.txt

 */

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.function.Function;

class Day01 {
    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();

        Scanner scanner = new Scanner(System.in);
        while (scanner.hasNextLine()) {
            lines.add(scanner.nextLine());
        }
        scanner.close();

        int result = processLines1(lines);
        System.out.println("Part 1: " + result);

        int result2 = processLines2(lines);
        System.out.println("Part 2: " + result2);
    }

    public static List<Integer> filterNums1(String str) {
        List<Integer> list = new ArrayList<>();

        for (char c : str.toCharArray()) {
            if (Character.isDigit(c)) {
                list.add(Character.getNumericValue(c));
            }
        }

        return list;
    }

    public static List<Integer> filterNums2(String str) {
        List<Integer> list = new ArrayList<>();

        for (int i = 0; i < str.length(); i++) {
            String frag = str.substring(i);
            Function<String, Boolean> startsWith = frag::startsWith;
            char c = str.charAt(i);
            if (Character.isDigit(c)) {
                list.add(Character.getNumericValue(c));
            } else {
                if (startsWith.apply("one")) {
                    list.add(1);
                } else if (startsWith.apply("two")) {
                    list.add(2);
                } else if (startsWith.apply("three")) {
                    list.add(3);
                } else if (startsWith.apply("four")) {
                    list.add(4);
                } else if (startsWith.apply("five")) {
                    list.add(5);
                } else if (startsWith.apply("six")) {
                    list.add(6);
                } else if (startsWith.apply("seven")) {
                    list.add(7);
                } else if (startsWith.apply("eight")) {
                    list.add(8);
                } else if (startsWith.apply("nine")) {
                    list.add(9);
                }
            }
        }

        return list;

    }

    private static int processLines(List<String> lines, Function<String, List<Integer>> filterFn) {
        int sum = 0;
        for (String line : lines) {
            final List<Integer> nums = filterFn.apply(line);

            int lineTotal = 0;
            lineTotal += nums.get(0) * 10;
            lineTotal += nums.get(nums.size() - 1); // potential crash here

            sum += lineTotal;
        }
        return sum;
    }

    public static int processLines1(List<String> lines) {
        return processLines(lines, Day01::filterNums1);
    }

    public static int processLines2(List<String> lines) {
        return processLines(lines, Day01::filterNums2);
    }

}
