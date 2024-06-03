CNat = All X. (X -> X) -> X -> X;
c0 = (lambda X. lambda s: X->X. lambda z: X. z) as CNat;
csucc = (lambda n: CNat. lambda X. lambda s: X->X. lambda z: X. s (n [X] s z)) as CNat -> CNat;

PairNat = All X. (CNat -> CNat -> X) -> X;

cnat2nat = lambda m: CNat. m [Nat] (lambda x: Nat. succ(x)) 0;

pairNat = lambda a: CNat. lambda b: CNat.
    lambda X. lambda f: (CNat -> CNat -> X).
        f a b;
fstNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        a);
sndNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        b);

/* End Prelude */

f = lambda p: PairNat.
    pairNat (csucc (fstNat p)) (fstNat p);

cpred = lambda n: CNat.
    sndNat (n [PairNat] f (pairNat c0 c0));

c3 = csucc (csucc (csucc c0));
cnat2nat (cpred c3);