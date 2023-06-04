interface None {
    readonly kind: 'None';
}

interface Some<A> {
    readonly kind: 'Some';
    readonly val: A;
}

type Option<A> = None | Some<A>;

export const none: None = { kind: 'None' };
export const some = function <A>(a: A): Option<A> {
    return { kind: 'Some', val: a };
};

function bind<A, B>(
    a: Option<A>,
    fn: (a: A) => Option<B>,
): Option<B> {
    switch (a.kind) {
        case 'None':
            return none;
        case 'Some':
            return fn(a.val);
        default:
            // eslint-disable-next-line no-case-declarations
            const exhaustiveCheck: never = a;
            return exhaustiveCheck;
    }
}

function map2<A, B, V>(
    fn: (a: A, b: B) => V,
    ma: Option<A>,
    mb: Option<B>
): Option<V> {
    return bind(ma, (a) => {
        return bind(mb, (b) => {
            return some(fn(a, b));
        });
    });
}

export function add(
    a: Option<number>,
    b: Option<number>
): Option<number> {
    return bind(a, (x) => {
        return bind(b, (y) => {
            return some(x + y);
        });
    });
}

export function mul(
    a: Option<number>,
    b: Option<number>
): Option<number> {
    return map2(
        (x, y) => x * y,
        a,
        b,
    );
}
