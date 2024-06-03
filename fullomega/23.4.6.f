CBool = All X. X -> X -> X;
tru = (lambda X. lambda t:X. lambda f: X. t) as CBool;
fls = (lambda X. lambda t:X. lambda f: X. f) as CBool;

CNat = All X. (X -> X) -> X -> X;
c0 = (lambda X. lambda s: X->X. lambda z: X. z) as CNat;
csucc = (lambda n: CNat. lambda X. lambda s: X->X. lambda z: X. s (n [X] s z)) as CNat -> CNat;

cbool_to_bool = lambda b: CBool. b [Bool] true false;

/* End prelude */

iszro = lambda n: CNat.
    n [CBool] (lambda b: CBool. fls) tru;

cbool_to_bool (iszro c0);
cbool_to_bool (iszro (csucc c0));
cbool_to_bool (iszro (csucc (csucc c0)));