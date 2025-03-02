package lib;

import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

public class Ex_003Test {
    @Test
    void noChange() {
        assertEquals("", Ex_003.solution(""));
        assertEquals("123", Ex_003.solution("123"));
    }

    @Test
    void addSeparators() {
        assertEquals("1,234", Ex_003.solution("1234"));
        assertEquals("12,345", Ex_003.solution("12345"));
    }

    @Test
    void decimals() {
        assertEquals("123.456", Ex_003.solution("123.456"));
        assertEquals("1,234.567", Ex_003.solution("1234.567"));
        assertEquals("12,345,678.9", Ex_003.solution("12345678.9"));
    }
}
