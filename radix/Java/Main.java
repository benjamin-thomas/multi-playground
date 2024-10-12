import java.util.Scanner;
import java.util.stream.IntStream;

/*

 java ./Main.java 16 < <(echo -e "ABC\nDEF")
 echo ./Main.java | entr -c bash -c 'java ./Main.java 16 < <(echo -e "ABC\nDEF")'
 echo ./Main.java | entr -c bash -c 'java ./Main.java 10 < <(echo -e "10\n11")'

 */

public class Main {

    static void runProgram(int base) {
        int i = 0;
        try (Scanner scanner = new Scanner(System.in)) {
            while (true) {
                if (!scanner.hasNextLine())
                    break;

                String line = scanner.nextLine();
                System.out.println(line);
                char[] chars = line.toCharArray();
                int result = i % 2 == 0 ? Radix.functional(base, line.chars()) : Radix.imperative(base, chars);
                System.out.printf(" -> %d\n\n", result);
                i++;
            }
        }
    }

    public static void main(String[] args) {
        int base = getArgOrDie(args, 0);

        runProgram(base);
    }

    private static int getArgOrDie(String[] args, int idx) {
        if (args.length <= idx) {
            System.err.println("Usage: java ./Main.java BASE");
            System.exit(1);
        }
        return Integer.parseInt(args[idx]);
    }

    static final class Radix {
        public static int digitOfChar(char c) {
            if (c >= '0' && c <= '9') {
                return c - '0';
            } else if (c >= 'A' && c <= 'Z') {
                return c - 'A' + 10;
            } else {
                throw new IllegalArgumentException("digitOfChar: " + c);
            }
        }

        static int imperative(int base, char[] lst) {
            var acc = 0;
            for (char c : lst) {
                int codePoint = digitOfChar(c);

                checkCodePoint(base, c, codePoint);

                acc = codePoint + acc * base;
            }
            return acc;
        }

        static int functional(int base, IntStream codePoints) {
            return codePoints
                    .map(n -> {
                        int codePoint = digitOfChar((char) n);
                        checkCodePoint(base, (char) n, codePoint);
                        return codePoint;
                    })
                    .reduce(0, (acc, n) -> n + base * acc);
        }

        private static void checkCodePoint(int base, char c, int codePoint) {
            if (codePoint < 0 || codePoint >= base)
                throw new IllegalArgumentException(String.format("Bad input %d -> %d", c, codePoint));
        }
    }
}
