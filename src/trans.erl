-module(trans).

-export([trans/2]).

trans({set, Imm, _}, {ref, Attr, P}) ->

    Type = case Attr of
        hp -> state;
        rem_moves -> state;
        _ -> attr
    end,

    #{Type:=#{Attr:=Orig}=TypeInstance} = P,

    ReferredImm = ref:ref(Imm),

    case is_number(ReferredImm) of
        true -> P#{Type:=TypeInstance#{Attr:=round(ReferredImm), diff:=round(Orig - ReferredImm)}};
        _    -> P#{Type:=TypeInstance#{Attr:=ReferredImm}}
    end;

trans({add, Inc, React}, {_, _, P}=ToWhom) when (React==absorbable) or (React==both) ->
    ArmorRatio = 1 - ref:ref({attr, armor, P}) / 10000,
    trans({set, ref:ref(ToWhom) + ref:ref(Inc) * ArmorRatio, none}, ToWhom);

trans({add, Inc, _}, ToWhom) ->
    trans({set, ref:ref(ToWhom) + ref:ref(Inc), none}, ToWhom);

trans({add_mul, Mul, Absorbing}, ToWhom) ->
    trans({add, ref:ref(ToWhom) * ref:ref(Mul), Absorbing}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, Absorbing}, ToWhom) ->
    trans({add, ref:ref(Inc) * ref:ref(Mul), Absorbing}, ToWhom).
