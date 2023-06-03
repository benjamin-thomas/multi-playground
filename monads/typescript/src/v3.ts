interface Err<T> {
    readonly kind: 'Err';
    readonly val: T;
}

interface Ok<T> {
    readonly kind: 'Ok';
    readonly val: T;
}

type Result<A, B> = Err<A> | Ok<B>;

export function err<T>(v: T): Err<T> {
    return { kind: 'Err', val: v };
}

export function ok<T>(v: T): Ok<T> {
    return { kind: 'Ok', val: v };
}

function bind<X, A, B>(
    result: Result<X, A>,
    callback: (a: A) => Result<X, B>)
    : Result<X, B> {
    if (result.kind === 'Err') {
        return err(result.val);
    } else {
        return callback(result.val);
    }
}

export function add(a: Result<string, number>, b: Result<string, number>): Result<string, number> {
    return bind(a, (x) => {
        return bind(b, (y) => {
            return ok(x + y);
        });
    });
}

function map2<A, B, V, X>(
    fn: (a: A, b: B) => V,
    resA: Result<X, A>,
    resB: Result<X, B>)
    : Result<X, V> {
    return bind(resA, (x) => {
        return bind(resB, (y) => {
            return ok(fn(x, y));
        });
    });
}

export function mul(a: Result<string, number>, b: Result<string, number>): Result<string, number> {
    return map2(
        (x, y) => x * y,
        a, b,
    );
}

