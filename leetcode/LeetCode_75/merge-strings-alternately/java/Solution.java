// echo Solution.java | entr -c java /_
class Solution {

    public String mergeAlternately(String word1, String word2) {
        var sb = new StringBuilder();

        int i = 0;
        while (i < word1.length() || i < word2.length()) {
            if (i < word1.length()) {
                sb.append(word1.charAt(i));
            }
            if (i < word2.length()) {
                sb.append(word2.charAt(i));
            }
            i++;
        }

        return sb.toString();
    }

    public static void main(String[] args) {
        var s = new Solution();
        assertEqual(s.mergeAlternately("ABC", "abc"), "AaBbCc");
        assertEqual(s.mergeAlternately("AB", "ab_cde"), "AaBb_cde");
        assertEqual(s.mergeAlternately("ABC_DE", "abc"), "AaBbCc_DE");
        assertEqual(s.mergeAlternately("ABC", ""), "ABC");
        assertEqual(s.mergeAlternately("", "abc"), "abc");
        assertEqual(s.mergeAlternately("", ""), "");
        System.err.println("OK");
    }

    public static <T> void assertEqual(T a, T b) {
        if (!a.equals(b)) {
            System.out.print("\u001B[31m");
            System.out.println("Test failed! Expected: " + b + ", Actual: " + a);
            System.out.print("\u001B[0m");
            System.exit(1);
        }
    }
}
