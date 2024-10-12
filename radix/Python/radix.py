#!/usr/bin/env python3

from functools import reduce


def greet(name: str) -> str:
    return f'Hello, {name}!'


acc = 0
for s in ['A', 'B', 'C']:
    acc = int(s, base=16) + 16*acc
print(acc)


print(
    reduce(
        lambda acc, s: int(s, base=16) + 16*acc,
        ['A', 'B', 'C'],
        0
    )
)

print(
    reduce(
        lambda acc,
        n: n + 16*acc,
        map(lambda s: int(s, base=16), ['A', 'B', 'C']),
        0
    )
)
