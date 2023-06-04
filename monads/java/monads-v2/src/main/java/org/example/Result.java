package org.example;

import java.util.function.Function;

public record Result<L, R>(L err, R ok) {
    public static <L, R> Result<L, R> err(L l) {
        return new Result<>(l, null);
    }

    public static <L, R> Result<L, R> ok(R r) {
        return new Result<>(null, r);
    }

    public static <A, B, X> Result<X, B> bind(
            Result<X, A> ra,
            Function<A, Result<X, B>> fn
    ) {
        if (ra.ok == null) {
            return err(ra.err);
        } else {
            return fn.apply(ra.ok);
        }
    }

    public static <A, B, C, X, V> Result<X, V> map3(
            TriFunction<A, B, C, V> fn,
            Result<X, A> ma,
            Result<X, B> mb,
            Result<X, C> mc
    ) {
        return bind(ma, a ->
                bind(mb, b ->
                        bind(mc, c ->
                                ok(fn.apply(a, b, c))
                        )));
    }

}
