pair = lambda X.lambda Y.lambda x:X.lambda y:Y.lambda R.lambda p:X->Y->R.p x y;

f = lambda X.lambda Y.lambda f:All R. (X->Y->R) -> R. f;

fst = lambda X.lambda Y.lambda p:All R. (X->Y->R) -> R.p [X] (lambda x:X.lambda y:Y.x);
snd = lambda X.lambda Y.lambda p:All R. (X->Y->R) -> R.p [Y] (lambda x:X.lambda y:Y.y);

pr = pair [Nat] [Bool] 0 false;
fst [Nat] [Bool] pr;
snd [Nat] [Bool] pr;

List = All R. (Nat->R->R) -> R -> R; 

diverge =
lambda X.
lambda _:Unit.
fix (lambda x:X. x);

nil = (lambda R. lambda c:Nat->R->R. lambda n:R. n)
    as List; 

cons = 
lambda hd:Nat. lambda tl: List.
    (lambda R. lambda c:Nat->R->R. lambda n:R. c hd (tl [R] c n))
    as List; 

isnil =  
lambda l: List. 
    l [Bool] (lambda hd:Nat. lambda tl:Bool. false) true; 

head = 
lambda l: List. 
    (l [Unit->Nat] (lambda hd:Nat. lambda tl:Unit->Nat. lambda _:Unit. hd) (diverge [Nat]))
    unit; 

tail =   
lambda l: List. 
    (fst [List] [List] ( 
    l [All R. (List->List->R) -> R]
        (lambda hd: Nat. lambda tl: All R. (List->List->R) -> R. 
        pair [List] [List] 
            (snd [List] [List] tl)  
            (cons hd (snd [List] [List] tl))) 
        (pair [List] [List] (nil) (nil))))
    as List;

/* end prelude */

stack = {
    *List,
    {
        new = nil,
        push = lambda item: Nat. lambda list: List. cons item list,
        top = lambda list: List. head list,
        pop = lambda list: List. tail list,
        isempty = lambda list: List. isnil list
    }
} as {Some Stack, { new: Stack, push: Nat->Stack->Stack, top: Stack->Nat, pop: Stack->Stack, isempty: Stack->Bool}};

let {Stack, ops} = stack in
    (ops.top (ops.pop (ops.pop (ops.push 3 (ops.push 2 (ops.push 1 ops.new))))));
let {Stack, ops} = stack in
    (ops.isempty (ops.pop (ops.pop (ops.push 3 (ops.push 2 (ops.push 1 ops.new))))));
let {Stack, ops} = stack in
    (ops.isempty (ops.pop (ops.pop (ops.pop (ops.push 3 (ops.push 2 (ops.push 1 ops.new)))))));