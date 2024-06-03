CBool = All X. X -> X -> X;
tru = (lambda X. lambda t:X. lambda f: X. t) as CBool;
fls = (lambda X. lambda t:X. lambda f: X. f) as CBool;

/* End prelude */

conjunction = lambda a: CBool. lambda b: CBool. lambda X.
    lambda t: X. lambda f: X.
        a [X] (b [X] t f) f;

cbool_to_bool = lambda b: CBool. b [Bool] true false;

cbool_to_bool (conjunction tru tru);
cbool_to_bool (conjunction tru fls);
cbool_to_bool (conjunction fls tru);
cbool_to_bool (conjunction fls fls);