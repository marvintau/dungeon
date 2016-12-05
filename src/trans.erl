-module(trans).

-export([trans/2]).

trans({set, Imm, _}, Ref) ->
	ref:set(Ref, ref:val(Imm));

trans({add, Inc, React}, {_, _, P}=ToWhom) when (React==absorbable) or (React==both) ->
    ArmorRatio = 1 - ref:val({attr, armor, P}) / 10000,
    trans({set, ref:val(ToWhom) + ref:val(Inc) * ArmorRatio, none}, ToWhom);

trans({add, Inc, _}, ToWhom) ->
    trans({set, ref:val(ToWhom) + ref:val(Inc), none}, ToWhom);

trans({add_mul, Mul, Absorbing}, ToWhom) ->
    trans({add, ref:val(ToWhom) * ref:val(Mul), Absorbing}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, Absorbing}, ToWhom) ->
    trans({add, ref:val(Inc) * ref:val(Mul), Absorbing}, ToWhom).
