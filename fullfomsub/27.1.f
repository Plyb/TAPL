SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit};
CounterRep = {x: Ref Nat};
InstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat};
InstrCounterRep = {x: Ref Nat, a: Ref Nat};

/* end prelude */

/* setCounterClass =
    lambda R<:CounterRep.
    lambda self: Source(R->SetCounter).
        lambda r: R. {
            get = lambda _:Unit. !(r.x),
            set = lambda i:Nat. r.x:=i,
            inc = lambda __:Unit. (!self r).set (succ((!self r).get unit))
        }; */