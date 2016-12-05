-module(trans).

-export([trans/2, trans/3]).

% ========================= TRANSFER INSTRUCTIONS ==============================
% Apply transfer operations over specific attributes of player context. The type
% could be varying state (hp, remaining moves) or attribute (hit, dodge, block,
% etc.) that resets for every round. The supported operations include get, set,
% add, add & multiply original value, or the value referring to other attributes.
%
% trans cares if the damage will be absorbed by the armor of defender.
%
% expecting {Opcode, Value, React} where opcode of set/add/add_mul/add_inc_mul,
% and Value of number, interval or {type, attribute, off/def} triple.


trans({set, Imm, _}, Ref) ->
	ref:set(Ref, Imm);

trans({add, Inc, React}, {_, _, P}=ToWhom) when (React==absorbable) or (React==both) ->
    ArmorRatio = 1 - ref:val({attr, armor, P}) / 10000,
    trans({set, ref:val(ToWhom) + Inc * ArmorRatio, none}, ToWhom);

trans({add, Inc, _}, ToWhom) ->
    trans({set, ref:val(ToWhom) + Inc, none}, ToWhom);

trans({add_mul, Mul, Absorbing}, ToWhom) ->
    trans({add, ref:val(ToWhom) * Mul, Absorbing}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, Absorbing}, ToWhom) ->
    trans({add, Inc * Mul, Absorbing}, ToWhom).


trans({{Opcode, Oper, AddCond}, {T, A, P}}, O, D) ->

    RefOperand = case Oper of
        {Ref1, Ref2} -> {ref:val(Ref1, O, D), ref:val(Ref2, O, D)};
        {Ref} -> ref:val(Ref, O, D)
    end,

    RefWhom = {T, A, ref:who_this(P, O, D)},

    IsResisted = case AddCond of
        resistable ->
                rand:uniform() * 120 < ref:get({attr, resist, D});
        both -> rand:uniform() * 120 < ref:get({attr, resist, D});
        _ -> false
    end,

    case {IsResisted, P} of
        {true, _} -> {resisted, {T, A, P}, O, D};
        {_, off} -> {effected, {T, A, P}, trans:trans({Opcode, RefOperand, AddCond}, RefWhom), D};
        {_, def} -> {effected, {T, A, P}, O, trans:trans({Opcode, RefOperand, AddCond}, RefWhom)}
    end.
