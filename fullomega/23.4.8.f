CNat = All X. (X -> X) -> X -> X;
c0 = (lambda X. lambda s: X->X. lambda z: X. z) as CNat;
csucc = (lambda n: CNat. lambda X. lambda s: X->X. lambda z: X. s (n [X] s z)) as CNat -> CNat;

PairNat = All X. (CNat -> CNat -> X) -> X;

cnat2nat = lambda m: CNat. m [Nat] (lambda x: Nat. succ(x)) 0;

/* End Prelude */

pairNat = lambda a: CNat. lambda b: CNat.
    lambda X. lambda f: (CNat -> CNat -> X).
        f a b;
fstNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        a);
sndNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        b);

c1 = csucc c0;

myPairNat = pairNat c0 c1;

cnat2nat (fstNat myPairNat);
cnat2nat (sndNat myPairNat);
