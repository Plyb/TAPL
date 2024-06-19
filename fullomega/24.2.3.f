Counter = {Some State, {state:State, methods: {get:State->Nat, inc:State->State}}};

counter = {
    *Nat,
    {
        state = 0,
        methods = {
            get = lambda x:Nat. x,
            inc = lambda x:Nat. succ(x)
        }
    }
} as Counter;

sendget = lambda c:Counter.
    let {State, body} = c in
        body.methods.get(body.state);

sendinc = lambda c:Counter.
    let {State, body} = c in {
            *State,
            {
                state = body.methods.inc(body.state),
                methods = body.methods
            }
        } as Counter;

/* end prelude */

not = lambda b: Bool. if b then false else true;

iseven = fix (lambda i: Nat -> Bool.
    lambda n:Nat.
        if (iszero n) then true else (not (i (pred n))));

FlipFlop = {Some State, {state: State, methods: {toggle: State->State, read: State->Bool, reset: State->State}}};

flipFlop = {
    *Counter,
    {
        state = counter,
        methods = {
            toggle = lambda c:Counter. sendinc c,
            read = lambda c:Counter. iseven (sendget c),
            reset = lambda c:Counter. counter
        }
    }
} as FlipFlop;

sendtoggle = lambda f:FlipFlop.
    let {State, body} = f in {
        *State,
        {
            state = body.methods.toggle(body.state),
            methods = body.methods
        }
    } as FlipFlop;

sendread = lambda f:FlipFlop.
    let {State, body} = f in
        body.methods.read(body.state);

sendreset = lambda f:FlipFlop.
    let {State, body} = f in {
        *State,
        {
            state = body.methods.reset(body.state),
            methods = body.methods
        }
    } as FlipFlop;

sendread flipFlop;
sendread (sendtoggle flipFlop);
sendread(sendtoggle (sendtoggle flipFlop));
sendread(sendreset(sendtoggle(flipFlop)));
sendread(sendreset(flipFlop));
