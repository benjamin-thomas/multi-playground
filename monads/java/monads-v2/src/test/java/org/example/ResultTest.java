package org.example;

import org.junit.jupiter.api.Test;

import static org.example.Result.bind;
import static org.example.Result.err;
import static org.example.Result.ok;
import static org.junit.jupiter.api.Assertions.*;

class ResultTest {

    private static Result<String, Integer> add(
            Result<String, Integer> ra,
            Result<String, Integer> rb,
            Result<String, Integer> rc
    ) {
        return bind(ra, a ->
                bind(rb, b ->
                        bind(rc, c ->
                                ok(a + b + c))));
    }

    private static Result<String, Integer> mul(
            Result<String, Integer> ra,
            Result<String, Integer> rb,
            Result<String, Integer> rc
    ) {
        return Result.map3(
                (a, b, c) -> a * b * c,
                ra, rb, rc
        );
    }

    @Test
    void equality() {
        assertEquals(err("wat"), err("wat"));
        assertNotEquals(err("wat"), err("wat2"));
        assertEquals(ok(1), ok(1));
        assertNotEquals(ok(1), ok(2));
    }

    @Test
    void add() {
        assertEquals(err("1st failed!"), add(err("1st failed!"), ok(2), ok(3)));
        assertEquals(err("2nd failed!"), add(ok(1), err("2nd failed!"), ok(3)));
        assertEquals(err("3rd failed!"), add(ok(1), ok(2), err("3rd failed!")));
        assertEquals(
                ok(12),
                add(ok(2), ok(4), ok(6))
        );
    }

    @Test
    void mul() {
        assertEquals(err("1st failed!"), mul(err("1st failed!"), ok(2), ok(3)));
        assertEquals(err("2nd failed!"), mul(ok(1), err("2nd failed!"), ok(3)));
        assertEquals(err("3rd failed!"), mul(ok(1), ok(2), err("3rd failed!")));
        assertEquals(
                ok(24),
                mul(ok(3), ok(4), ok(2))
        );
    }


}