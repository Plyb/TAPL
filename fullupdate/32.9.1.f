CounterR = {#x:Nat};
CounterM = lambda R. {get: R->Nat, inc:R->R};
Object = lambda M::*=>*. {Some X, {state:X, methods:M X}};
Counter = Object CounterM;

counterClass =
    lambda R<:CounterR.
    lambda self: Unit-> CounterM R.
    lambda _:Unit.
        {get = lambda s:R. s.x,
            inc = lambda s:R. s<-x=succ(s.x)}
        as CounterM R;

SetCounterM = lambda R. {get: R->Nat, set:R->Nat->R, inc:R->R};

setCounterClass =
    lambda R<:CounterR.
    lambda self: Unit->SetCounterM R.
    lambda _:Unit.
        let super = counterClass [R] self unit in
            {get = super.get,
                set = lambda s:R. lambda n: Nat. s<-x=n,
                inc = lambda s:R. (self unit).set s (succ((self unit).get s))}
        as SetCounterM R;

InstrCounterM =
    lambda R. {get: R->Nat, set:R->Nat->R, inc:R->R, accesses: R->Nat};
InstrCounterR = {#x:Nat,#count:Nat};

instrCounterClass =
    lambda R<:InstrCounterR.
    lambda self: Unit->InstrCounterM R.
    lambda _:Unit.
        let super = setCounterClass [R] self unit in
        {get = super.get,
        set = lambda s:R. lambda n:Nat.
            let r = super.set s n in
            r<-count=succ(r.count),
        inc = super.inc,
        accesses = lambda s:R. s.count}
    as InstrCounterM R;

/* end of prelude */

BackupCounterM =
    lambda R. {get: R->Nat, set:R->Nat->R, inc:R->R, accesses: R->Nat, backup: R->R, reset: R->R};
BackupCounterR = {#x:Nat,#count:Nat,#backup:Nat};


backupCounterClass =
    lambda R<:BackupCounterR.
    lambda self: Unit->BackupCounterM R.
    lambda _:Unit.
        let super = instrCounterClass [R] self unit in
        {get = super.get,
        set = super.set,
        inc = super.inc,
        accesses = super.accesses,
        backup = lambda s:R. s<-backup=s.x,
        reset = lambda s:R. s<-x=s.backup}
    as InstrCounterM R;