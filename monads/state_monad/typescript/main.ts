import * as S from 'fp-ts/State';

/*
    Conclusion: I found this library too hard to use. I don't like it at the moment.
    This is mostly due to TypeScript probably. It's pretty hard to line-up the types, lots of (silent or unhelpful) gotchas.
*/

type Random<A> = S.State<bigint, A>;

const random = (seed: bigint): bigint => {
    const a = BigInt(1103515245);
    const c = BigInt(12345);
    const m = BigInt(2 ** 31);
    return (a * seed + c) % m;
};

const randNum: Random<bigint> = S.flatMap(S.get(), (seed) => {
    const newSeed = random(seed);
    return S.flatMap(S.put(newSeed), (_: void) => {
        return S.of(newSeed);
    });
});

const randTup3 =
    S.flatMap(randNum, (a) =>
        S.flatMap(randNum, (b) =>
            S.flatMap(randNum, (c) => {
                return S.of([a, b, c]);
            })));


const fst = <A, B>([a, _]: [A, B]) => a;
const startSeed = BigInt(0);

console.log({ randNum: fst(randNum(startSeed)), randTup3: fst(randTup3(startSeed)) });
