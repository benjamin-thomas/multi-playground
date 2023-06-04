package org.example;

import java.util.function.Function;

public record Maybe<T>(T val) {
    public static <T> Maybe<T> just(T val) {
        return new Maybe<>(val);
    }

    public static <T> Maybe<T> nothing() {
        return new Maybe<>(null);
    }

    public static <A, B> Maybe<B> bind(
            Maybe<A> ma,
            Function<A, Maybe<B>> fn
    ) {
        if (ma.val == null) {
            return nothing();
        } else {
            return fn.apply(ma.val);
        }
    }

    public static <A, B, C, V> Maybe<V> map3(
            TriFunction<A, B, C, V> fn,
            Maybe<A> ma,
            Maybe<B> mb,
            Maybe<C> mc
    ) {
        return bind(ma, a ->
                bind(mb, b ->
                        bind(mc, c ->
                                just(fn.apply(a, b, c))
                        )));
    }

}
