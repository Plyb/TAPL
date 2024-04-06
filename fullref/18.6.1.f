CounterRep = {x: Ref Nat};

counterClass = lambda r:CounterRep.
  { get = lambda _:Unit. !(r.x),
    inc = lambda _:Unit. r.x:=succ(!(r.x))};

resetCounterClass = lambda r:CounterRep.
  let super = counterClass r in
    { get = super.get,
      inc = super.inc,
      reset = lambda _:Unit. r.x:=1};

decCounterClass = lambda r:CounterRep.
  let super = resetCounterClass r in
    { get = super.get,
      inc = super.inc,
      reset = super.reset,
      dec = lambda _:Unit. r.x:=pred(!(r.x))};

newDecCounter = lambda _:Unit.
  let r = {x=ref 1} in decCounterClass r;

decCounter = newDecCounter unit;

decCounter.get unit;
decCounter.inc unit;
decCounter.inc unit;
decCounter.inc unit;
decCounter.get unit;
decCounter.dec unit;
decCounter.get unit;
decCounter.reset unit;
decCounter.get unit;
