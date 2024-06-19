/* CBool = All X. X -> X -> X; */

/* end prelude */

SBool = All R <: Top. All T <: R. All F <: R. T -> F -> R;

STrue = All R <: Top. All T <: R. All F <: R. T -> F -> T;
SFalse = All R <: Top. All T <: R. All F <: R. T -> F -> F;

tru = (lambda R <: Top. lambda T <: R. lambda F <: R. lambda t: T. lambda f: F. t) as STrue;
fls = (lambda R <: Top. lambda T <: R. lambda F <: R. lambda t: T. lambda f: F. f) as SFalse;

not = lambda b: SBool.
    (lambda R <: Top. lambda T <: R. lambda F <: R. lambda t: T. lambda f: F.
        b [R] [F] [T] f t) as SBool;

nottf = lambda b: STrue.
    (lambda R <: Top. lambda T <: R. lambda F <: R. lambda t: T. lambda f: F.
        b [R] [F] [T] f t) as SFalse;
notft = lambda b: SFalse.
    (lambda R <: Top. lambda T <: R. lambda F <: R. lambda t: T. lambda f: F.
        b [R] [F] [T] f t) as STrue;


not tru;
not fls;
nottf tru;
notft fls;
