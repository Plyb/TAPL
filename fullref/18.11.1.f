CounterRep = {x: Ref Nat};

SetCounter = {
    get: Unit->Nat,
    set: Nat->Unit,
    inc: Unit->Unit
};

InstrCounterRep = {
    x: Ref Nat,
    a: Ref Nat
};

InstrCounter = {
    get: Unit->Nat,
    set: Nat->Unit,
    inc: Unit->Unit,
    accesses: Unit->Nat
};

setCounterClass = lambda r:CounterRep.
  lambda self: Unit->SetCounter.
    lambda _:Unit. {
      get = lambda _:Unit. !(r.x),
      set = lambda i:Nat. r.x:=i,
      inc = lambda _:Unit. (self unit).set(succ((self unit).get unit))
    };

instrCounterClass = lambda r:InstrCounterRep.
  lambda self:Unit -> InstrCounter.
    lambda _:Unit.
      let super = setCounterClass r self unit in {
        get = lambda _:Unit. (r.a:=succ(!(r.a)); super.get unit),
        set = lambda i:Nat. (r.a:=succ(!(r.a)); super.set i),
        inc = super.inc,
        accesses = lambda _:Unit. !(r.a)
      };

newInstrCounter = lambda _:Unit.
  let r = { x = ref 1, a = ref 0 } in
    fix (instrCounterClass r) unit;

instrCounter = newInstrCounter unit;

instrCounter.set 12;
instrCounter.get unit;
instrCounter.accesses unit;

ResetCounter = {
    get: Unit->Nat,
    set: Nat->Unit,
    inc: Unit->Unit,
    reset: Unit->Unit,
    accesses: Unit->Nat
};

resetCounterClass = lambda r:InstrCounterRep.
  lambda self: Unit->ResetCounter.
    lambda _: Unit.
      let super = instrCounterClass r self unit in {
        get = super.get,
        set = super.set,
        inc = super.inc,
        reset = lambda _:Unit. ((self unit).set 1),
        accesses = super.accesses
      };

newResetCounter = lambda _:Unit.
  let r = { x = ref 1, a = ref 0 } in
    fix (resetCounterClass r) unit;

resetCounter = newResetCounter unit;

resetCounter.inc unit;
resetCounter.reset unit;
resetCounter.get unit;
resetCounter.accesses unit;

BackupCounterRep = {
    x: Ref Nat,
    a: Ref Nat,
    b: Ref Nat
};

BackupCounter = {
    get: Unit->Nat,
    set: Nat->Unit,
    inc: Unit->Unit,
    reset: Unit->Unit,
    backup: Unit->Unit,
    accesses: Unit->Nat
};

backupCounterClass = lambda r:BackupCounterRep.
  lambda self: Unit->BackupCounter.
    lambda _: Unit.
      let super = resetCounterClass r self unit in {
        get = super.get,
        set = super.set,
        inc = super.inc,
        reset = lambda _:Unit. ((self unit).set (!(r.b))),
        backup = lambda _:Unit. r.b:=((self unit).get unit),
        accesses = super.accesses
      };

newBackupCounter = lambda _:Unit.
  let r = { x = ref 1, a = ref 0, b = ref 1 } in
    fix (backupCounterClass r) unit;

backupCounter = newBackupCounter unit;

backupCounter.set 5;
backupCounter.backup unit;
backupCounter.set 10;
backupCounter.reset unit;
backupCounter.get unit;
backupCounter.accesses unit;
