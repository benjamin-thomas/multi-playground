package lib;

public class Ex_003 {
    public static String solution(String str) {
        int dotPos = str.indexOf(".");
        int relDotPos = dotPos == -1 ? 0 : str.length() - dotPos;
        StringBuilder sb = new StringBuilder();

        for (int i = str.length() - 1; i >= 0; i--) {
            char c = str.charAt(i);
            sb.append(c);

            int relI = str.length() - i;

            if (relI > relDotPos) {
                if (i > 0 && (relI - relDotPos) % 3 == 0) {
                    sb.append(",");
                }
            }
        }
        str = sb.reverse().toString();
        return str;
    }
}
