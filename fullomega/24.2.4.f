Counter = {Some State, {state: State, methods: {get: State->Nat, inc: State->Unit}}};

newcounter = lambda _: Unit. {
    *Ref Nat,
    {
        state = ref 0,
        methods = {
            get = lambda n: Ref Nat. !n,
            inc = lambda n: Ref Nat. n := succ (!n)
        }
    }
} as Counter;

sendget = lambda c:Counter.
    let {State, body} = c in
        body.methods.get(body.state);

sendinc = lambda c:Counter.
    let {State, body} = c in
        body.methods.inc(body.state);

counter = newcounter unit;
sendinc counter;
sendget counter;
sendinc counter;
sendget counter;
sendinc counter;
sendget counter;

