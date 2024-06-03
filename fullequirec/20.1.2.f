Stream = Rec A. Unit->{Nat, A};

hd = lambda s:Stream. (s unit).1;
tl = lambda s:Stream. (s unit).2;

plus = fix (lambda p:Nat->Nat->Nat. 
lambda m:Nat. lambda n:Nat. 
if iszero m then n else succ (p (pred m) n));

fibonacci = fix (lambda f: Nat -> Nat -> Stream.
    lambda n: Nat. lambda m: Nat. lambda _:Unit.
        {m, f m (plus n m)}) 0 1;

hd fibonacci;
hd (tl fibonacci);
hd (tl (tl fibonacci));
hd (tl (tl (tl fibonacci)));
hd (tl (tl (tl (tl fibonacci))));
hd (tl (tl (tl (tl (tl fibonacci)))));
hd (tl (tl (tl (tl (tl (tl fibonacci))))));