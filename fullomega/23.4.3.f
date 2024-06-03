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
   lambda l: List X. 
     (l [Unit->X] (lambda hd:X. lambda tl:Unit->X. lambda _:Unit. hd) (diverge [X]))
     unit; 
 
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

/* end prelude */

last = lambda X.
  fix (lambda lst: List X -> X.
    lambda l: List X.
      if isnil [X] (tail [X] l)
        then head [X] l
        else lst (tail [X] l));

all_but_last = lambda X.
  fix (lambda abl: List X -> List X.
    lambda l: List X.
      if isnil [X] l
        then l
        else if isnil[X] (tail [X] l)
          then nil [X]
          else if isnil [X] (tail [X] (tail [X] l))
            then cons [X] (head [X] l) (nil [X])
            else cons [X] (head [X] l) (abl (tail [X] l)));
      

reverse = lambda X.
  fix (lambda r: List X -> List X.
      lambda l: List X.
          if isnil [X] l
              then l
              else cons [X]
                  (last [X] l)
                  (r (all_but_last [X] l)));
                    
test_list = cons [Nat] 3 (cons [Nat] 2 (cons [Nat] 1 (nil [Nat])));
(all_but_last [Nat] test_list);
(last [Nat] (cons [Nat] 3 (cons [Nat] 2 (nil [Nat]))));
(all_but_last [Nat] (cons [Nat] 3 (cons [Nat] 2 (nil [Nat]))));
(last [Nat] (cons [Nat] 3 (nil [Nat])));
(all_but_last [Nat] (cons [Nat] 3 (nil [Nat])));
(reverse [Nat] test_list);