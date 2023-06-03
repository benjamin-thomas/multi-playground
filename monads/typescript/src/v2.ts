interface None {
    readonly kind: 'None';
}

interface Some<A> {
    readonly kind: 'Some';
    readonly val: A;
}

type Option<A> = None | Some<A>;

export const none: None = {kind: 'None'};
export const some = function <A>(a: A): Option<A> {
    return {kind: 'Some', val: a};
};

function bind<A, B>(a: Option<A>,
                    fn: (a: A) => Option<B>): Option<B> {
    if (a.kind === 'None') {
        return none;
    } else {
        return fn(a.val);
    }
}

function map2<A, B, V>(
    fn: (a: A, b: B) => V,
    a: Option<A>,
    b: Option<B>)
    : Option<V> {
    if (a.kind === 'None') {
        return none;
    } else {
        if (b.kind === 'None') {
            return none;
        } else {
            return some(fn(a.val, b.val));
        }
    }
}


export function add(a: Option<number>, b: Option<number>): Option<number> {
    return bind(a, (x) => {
        return bind(b, (y) => {
            return some(x + y);
        });
    });
}

export function mul(a: Option<number>, b: Option<number>): Option<number> {
    return map2(
        (x, y) => x * y,
        a, b);
}


