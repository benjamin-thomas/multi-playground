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

            char c = str.charAt(i);
            if (Character.isDigit(c)) {
                list.add(Character.getNumericValue(c));
            } else {
                if (isOne(str, i)) {
                    list.add(1);
                } else if (isTwo(str, i)) {
                    list.add(2);
                } else if (isThree(str, i)) {
                    list.add(3);
                } else if (isFour(str, i)) {
                    list.add(4);
                } else if (isFive(str, i)) {
                    list.add(5);
                } else if (isSix(str, i)) {
                    list.add(6);
                } else if (isSeven(str, i)) {
                    list.add(7);
                } else if (isEight(str, i)) {
                    list.add(8);
                } else if (isNine(str, i)) {
                    list.add(9);
                }
            }
        }

        return list;

    }

    private static boolean isOne(String str, int i) {
        return str.length() - i > 2
                && str.charAt(i + 0) == 'o'
                && str.charAt(i + 1) == 'n'
                && str.charAt(i + 2) == 'e';
    }

    private static boolean isTwo(String str, int i) {
        return str.length() - i > 2
                && str.charAt(i + 0) == 't'
                && str.charAt(i + 1) == 'w'
                && str.charAt(i + 2) == 'o';
    }

    private static boolean isThree(String str, int i) {
        return str.length() - i > 4
                && str.charAt(i + 0) == 't'
                && str.charAt(i + 1) == 'h'
                && str.charAt(i + 2) == 'r'
                && str.charAt(i + 3) == 'e'
                && str.charAt(i + 4) == 'e';
    }

    private static boolean isFour(String str, int i) {
        return str.length() - i > 3
                && str.charAt(i + 0) == 'f'
                && str.charAt(i + 1) == 'o'
                && str.charAt(i + 2) == 'u'
                && str.charAt(i + 3) == 'r';
    }

    private static boolean isFive(String str, int i) {
        return str.length() - i > 3
                && str.charAt(i + 0) == 'f'
                && str.charAt(i + 1) == 'i'
                && str.charAt(i + 2) == 'v'
                && str.charAt(i + 3) == 'e';
    }

    private static boolean isSix(String str, int i) {
        return str.length() - i > 2
                && str.charAt(i + 0) == 's'
                && str.charAt(i + 1) == 'i'
                && str.charAt(i + 2) == 'x';
    }

    private static boolean isSeven(String str, int i) {
        return str.length() - i > 4
                && str.charAt(i + 0) == 's'
                && str.charAt(i + 1) == 'e'
                && str.charAt(i + 2) == 'v'
                && str.charAt(i + 3) == 'e'
                && str.charAt(i + 4) == 'n';
    }

    private static boolean isEight(String str, int i) {
        return str.length() - i > 4
                && str.charAt(i + 0) == 'e'
                && str.charAt(i + 1) == 'i'
                && str.charAt(i + 2) == 'g'
                && str.charAt(i + 3) == 'h'
                && str.charAt(i + 4) == 't';
    }

    private static boolean isNine(String str, int i) {
        return str.length() - i > 3
                && str.charAt(i + 0) == 'n'
                && str.charAt(i + 1) == 'i'
                && str.charAt(i + 2) == 'n'
                && str.charAt(i + 3) == 'e';
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
