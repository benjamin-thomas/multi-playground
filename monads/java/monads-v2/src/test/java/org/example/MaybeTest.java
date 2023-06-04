package org.example;

import org.junit.jupiter.api.Test;

import static org.example.Maybe.*;
import static org.junit.jupiter.api.Assertions.*;

class MaybeTest {

    Maybe<Integer> add(
            Maybe<Integer> ma,
            Maybe<Integer> mb
    ) {
        return bind(ma, a ->
                bind(mb, b ->
                        just(a + b)));
    }

    Maybe<Integer> mul(
            Maybe<Integer> ma,
            Maybe<Integer> mb,
            Maybe<Integer> mc
    ) {
        return map2(
                (a, b, c) -> a * b * c,
                ma, mb, mc
        );
    }

    @Test
    void equality() {
        assertEquals(nothing(), nothing());
        assertEquals(just(1), just(1));
    }

    @Test
    void add() {
        assertEquals(nothing(), add(nothing(), nothing()));
        assertEquals(nothing(), add(just(1), nothing()));
        assertEquals(nothing(), add(nothing(), just(2)));
        assertEquals(just(3), add(just(1), just(2)));
    }

    @Test
    void mul() {
        assertEquals(nothing(), mul(nothing(), nothing(), nothing()));
        assertEquals(nothing(), mul(just(1), nothing(), nothing()));
        assertEquals(nothing(), mul(nothing(), nothing(), just(3)));

        assertEquals(
                just(24),
                mul(
                        just(3),
                        just(4),
                        just(2)
                )
        );
    }
}