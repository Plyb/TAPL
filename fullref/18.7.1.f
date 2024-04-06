CounterRep = {x: Ref Nat};

counterClass = lambda r:CounterRep.
  { get = lambda _:Unit. !(r.x),
    inc = lambda _:Unit. r.x:=succ(!(r.x))};

resetCounterClass = lambda r:CounterRep.
  let super = counterClass r in
    { get = super.get,
      inc = super.inc,
      reset = lambda _:Unit. r.x:=1};

BackupCounter = {
  get: Unit->Nat, 
  inc: Unit->Unit,
  reset: Unit->Unit,
  backup: Unit->Unit
};

BackupCounterRep = {
  x: Ref Nat,
  b: Ref Nat
};

backupCounterClass = lambda r:BackupCounterRep.
  let super = resetCounterClass r in {
    get = super.get,
    inc = super.inc,
    reset = lambda _:Unit. r.x:=!(r.b),
    backup = lambda _:Unit. r.b:=!(r.x)
  };

DoubleBackupCounter = {
  get: Unit->Nat, 
  inc: Unit->Unit,
  reset: Unit->Unit,
  backup: Unit->Unit,
  reset2: Unit->Unit,
  backup2: Unit->Unit
};

DoubleBackupCounterRep = {
  x: Ref Nat,
  b: Ref Nat,
  bb: Ref Nat
};

doubleBackupCounterClass = lambda r:DoubleBackupCounterRep.
  let super = backupCounterClass r in {
    get = super.get,
    inc = super.inc,
    reset = super.reset,
    backup = super.backup,
    reset2 = lambda _:Unit. r.x:=!(r.bb),
    backup2 = lambda _:Unit. r.bb:=!(r.x)
  };

newDoubleBackupCounter = lambda _:Unit.
  let r = { x = ref 1, b = ref 1, bb = ref 1} in doubleBackupCounterClass r;

doubleBackupCounter = newDoubleBackupCounter unit;

doubleBackupCounter.inc unit;
doubleBackupCounter.backup unit;
doubleBackupCounter.inc unit;
doubleBackupCounter.backup2 unit;
doubleBackupCounter.inc unit;
doubleBackupCounter.get unit;
doubleBackupCounter.reset unit;
doubleBackupCounter.get unit;
doubleBackupCounter.reset2 unit;
doubleBackupCounter.get unit;
