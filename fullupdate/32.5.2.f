CounterR = {x:Nat};
CounterM = lambda R. {get: R->Nat, inc:R->R};
Object = lambda M::*=>*. {Some X, {state:X, methods:M X}};
Counter = Object CounterM;

ResetCounterM = lambda R. {get: R->Nat, inc:R->R, reset:R->R};
ResetCounter = Object ResetCounterM;

sendinc = lambda M<:CounterM. lambda c:Object M.
    let {X, b} = c in
        {*X,
            {state = b.methods.inc(b.state),
            methods = b.methods}}
        as Object M;

/* end of prelude */


sendget = lambda M<:CounterM. lambda c:Object M.
    let {X, b} = c in
        b.methods.get(b.state);
        
sendreset = lambda M<:ResetCounterM. lambda c:Object M.
    let {X, b} = c in
        {*X,
            {state = b.methods.reset(b.state),
            methods = b.methods}}
        as Object M;
