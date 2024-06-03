D = Rec X. <nat: Nat, fn: X->X, rcd: Nat->X>;

fix_D = 
lambda f:D->D.
  (lambda x:(Rec A.A->D). f (x x))
  (lambda x:(Rec A.A->D). f (x x));

diverge_D = lambda _:Unit. fix_D (lambda x:D. x);

eql = lambda eq:Nat -> Nat -> Bool. lambda m:Nat. lambda n:Nat.
    if iszero m then iszero n
    else if iszero n then false
    else eq (pred m) (pred n);


equal = fix eql;

/* my code below */

empty_rcd = <fn=lambda _:D. diverge_D unit> as D;

cons_rcd = lambda old_rcd:D. lambda new_key:D. lambda val:D.
    case new_key of
        <nat=n> ==> <rcd=lambda key:Nat.
            (if equal n key
                then val
                else case old_rcd of
                    <nat=n> ==> diverge_D unit
                    | <fn=f> ==> diverge_D unit
                    | <rcd=old_rcd> ==> old_rcd key)> as D
        | <fn=f> ==> diverge_D unit
        | <rcd=r> ==> diverge_D unit;

proj = lambda rcd:D. lambda key:D.
    case rcd of
        <nat=n> ==> diverge_D unit
        | <fn=f> ==> diverge_D unit
        | <rcd=r> ==> (case key of
            <nat=n> ==> r n
            | <fn=f> ==> diverge_D unit
            | <rcd=r> ==> diverge_D unit);

(cons_rcd empty_rcd (<nat=1> as D) (<nat=2> as D));

proj
    (cons_rcd (cons_rcd empty_rcd (<nat=1> as D) (<nat=2> as D)) (<nat=3> as D) (<nat=4> as D))
    (<nat=1> as D);