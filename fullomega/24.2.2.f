counter = {
    *Ref Nat,
    {
        new = lambda _:Unit. ref 0,
        get = lambda i:Ref Nat. !i,
        inc = lambda i:Ref Nat. i := succ(!i)
    }
} as {Some Counter, {new: Unit->Counter, get: Counter->Nat, inc: Counter->Unit}};

seq = lambda X. lambda _:Unit. lambda res:X. res;

let {Counter, ops} = counter in (
    let counter = ops.new unit in (
        seq [Nat] (ops.inc counter)
        (seq [Nat] (ops.inc counter)
        (seq [Nat] (ops.inc counter)
        (ops.get counter)))
    )
);
