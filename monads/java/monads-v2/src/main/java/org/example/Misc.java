package org.example;

@FunctionalInterface
interface TriFunction<A, B, C, V> {
    V apply(A a, B b, C c);
}