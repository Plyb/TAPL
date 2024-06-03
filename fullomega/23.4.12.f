 Pair = lambda X. lambda Y. All R. (X->Y->R) -> R;
 
 pair = lambda X.lambda Y.lambda x:X.lambda y:Y.lambda R.lambda p:X->Y->R.p x y;
 
 f = lambda X.lambda Y.lambda f:Pair X Y. f;
 
 fst = lambda X.lambda Y.lambda p:Pair X Y.p [X] (lambda x:X.lambda y:Y.x);
 snd = lambda X.lambda Y.lambda p:Pair X Y.p [Y] (lambda x:X.lambda y:Y.y);
 
 pr = pair [Nat] [Bool] 0 false;
 fst [Nat] [Bool] pr;
 snd [Nat] [Bool] pr;
 
 List = lambda X. All R. (X->R->R) -> R -> R; 
 
 diverge =
 lambda X.
   lambda _:Unit.
   fix (lambda x:X. x);
 
 nil = lambda X.
       (lambda R. lambda c:X->R->R. lambda n:R. n)
       as List X; 
 
 cons = 
 lambda X.
   lambda hd:X. lambda tl: List X.
      (lambda R. lambda c:X->R->R. lambda n:R. c hd (tl [R] c n))
      as List X; 
 
 isnil =  
 lambda X. 
   lambda l: List X. 
     l [Bool] (lambda hd:X. lambda tl:Bool. false) true; 
 
 head = 
 lambda X. 
   lambda l: List X. lambda default: X.
     l [X] (lambda hd:X. lambda tl:X. hd) default; 
 
 tail =  
 lambda X.  
   lambda l: List X. 
     (fst [List X] [List X] ( 
       l [Pair (List X) (List X)]
         (lambda hd: X. lambda tl: Pair (List X) (List X). 
           pair [List X] [List X] 
             (snd [List X] [List X] tl)  
             (cons [X] hd (snd [List X] [List X] tl))) 
         (pair [List X] [List X] (nil [X]) (nil [X]))))
     as List X;

CBool = All X. X -> X -> X;
tru = (lambda X. lambda t:X. lambda f: X. t) as CBool;
fls = (lambda X. lambda t:X. lambda f: X. f) as CBool;

CNat = All X. (X -> X) -> X -> X;
c0 = (lambda X. lambda s: X->X. lambda z: X. z) as CNat;
csucc = (lambda n: CNat. lambda X. lambda s: X->X. lambda z: X. s (n [X] s z)) as CNat -> CNat;

PairNat = All X. (CNat -> CNat -> X) -> X;

pairNat = lambda a: CNat. lambda b: CNat.
    lambda X. lambda f: (CNat -> CNat -> X).
        f a b;
fstNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        a);
sndNat = lambda p: PairNat.
    p [CNat] (lambda a: CNat. lambda b: CNat.
        b);

cpred_help = lambda p: PairNat.
    pairNat (csucc (fstNat p)) (fstNat p);

cpred = lambda n: CNat.
    sndNat (n [PairNat] cpred_help (pairNat c0 c0));
    
iszro = lambda n: CNat.
    n [CBool] (lambda b: CBool. fls) tru;
    
cbool_to_bool = lambda b: CBool. b [Bool] true false;

conjunction = lambda a: CBool. lambda b: CBool. lambda X.
    lambda t: X. lambda f: X.
        a [X] (b [X] t f) f;

cnat2nat = lambda m: CNat. m [Nat] (lambda x: Nat. succ(x)) 0;

/* End Prelude */

map = lambda X. lambda Y. lambda l: List X. lambda f: X -> Y.
    l [List Y]
        (lambda curr: X. lambda prev: List Y. cons [Y] (f curr) prev)
        (nil [Y]);

cnat_list_to_nat_list = lambda l: List CNat. map [CNat] [Nat] l cnat2nat;

and = lambda a: Bool. lambda b: Bool.
    if a
        then b
        else false;
not = lambda b: Bool.
    if b
        then false
        else true;

less_than = lambda a: CNat. lambda b: CNat.
    and
        (cbool_to_bool (iszro (b [CNat] cpred a)))
        (not (cbool_to_bool (iszro (a [CNat] cpred b))));

insert = lambda X. lambda comp: (X -> X -> Bool). lambda l: List X. lambda elem: X.
    (lambda res: Pair (List X) Bool.
        if not (snd [List X] [Bool] res)
            then cons [X] elem (nil [X])
            else fst [List X] [Bool] res)
    (l [Pair (List X) Bool]
        (lambda curr: X. lambda prev: Pair (List X) Bool.
            if and (comp curr elem) (not (snd [List X] [Bool] prev))
                then pair [List X] [Bool] (cons [X] curr (cons [X] elem (fst [List X] [Bool] prev))) true
                else pair [List X] [Bool] (cons [X] curr (fst [List X] [Bool] prev)) (snd [List X] [Bool] prev))
        (pair [List X] [Bool] (nil [X]) false));

c1 = csucc c0;
c2 = csucc c1;
c3 = csucc c2;
c4 = csucc c3;
c5 = csucc c4;

less_than c1 c2;
less_than c2 c1;
less_than c1 c1;

list_with_hole = cons [CNat] c1 (cons [CNat] c2 (cons [CNat] c4 (cons [CNat] c5 (nil [CNat]))));
cnat_list_to_nat_list (insert [CNat] less_than list_with_hole c3);
list_with_repetitions = cons [CNat] c1 (cons [CNat] c2 (cons [CNat] c2 (cons [CNat] c3 (nil [CNat]))));
cnat_list_to_nat_list (insert [CNat] less_than list_with_repetitions c2);
empty_list = nil [CNat];
cnat_list_to_nat_list (insert [CNat] less_than empty_list c1);

sort = lambda X. lambda comp: (X -> X -> Bool). lambda l: List X.
    l [List X]
        (lambda curr: X. lambda prev: List X.
            insert [X] comp prev curr)
        (nil [X]);

reversed_list = cons [CNat] c3 (cons [CNat] c2 (cons [CNat] c1 (nil [CNat])));
cnat_list_to_nat_list (sort [CNat] less_than reversed_list);
