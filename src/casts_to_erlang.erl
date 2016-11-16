-module(casts_to_erlang).

-author('Yue Marvin Tao').

-export([casts/1]).

ref({[{_, Type}, {_, Attribute}, {_, Role}]}) ->
    {binary_to_atom(Type, utf8), binary_to_atom(Attribute, utf8), binary_to_atom(Role, utf8) };
ref({[{_, Min}, {_, Max}]}) ->
    {Min, Max};
ref({[{_, Value}]}) ->
    Value.

operand({[{_, Inc}, {_, Mul}]}) -> {ref(Inc), ref(Mul)};
operand({[{_, Inc}]}) -> ref(Inc).

operator({[{_, OpCode}, {_, Operand}, {_, Note}]}) ->
    {binary_to_atom(OpCode, utf8), operand(Operand), binary_to_atom(Note, utf8) }.

trans({[{_, Operator}, {_, To}]}) ->
    {operator(Operator), ref(To)}.

trans_list(TransList) ->
    [trans(Trans) || Trans <- TransList].

comp_cond({[{_, Value}, {_, Op}, {_, Ref}]}) ->

    ParsedValue = case is_number(Value) of
        true -> Value;
        _ -> binary_to_atom(Value, utf8)
    end,

    {ParsedValue, binary_to_atom(Op, utf8), ref(Ref)}.

comp_cond_list(CompCondList) ->
    [comp_cond(CompCond) || CompCond <- CompCondList].

seq_cond({[{_, Start}, {_, Last}, {_, Stage}]}) ->
    {Start, Last, binary_to_atom(Stage, utf8)}.

conds({[{_, SeqCond}, {_, CompCondList}]}) ->
    {seq_cond(SeqCond), comp_cond_list(CompCondList)}.

single_effect({[{_, Cond}, {_, TransList}]}) ->
    {conds(Cond), trans_list(TransList)}.

effects(Effects) ->
    [single_effect(Effect) || Effect <- Effects].

single_group({[{_, Prob}, {_, Effects}]}) ->
    {Prob, effects(Effects)}.

groups(Groups) ->
   [single_group(Group) || Group <- Groups]. 

cast(CastData) ->
    % error_logger:info_report(CastData).
    {[{_, Name}, {_, Class}, {_, Groups}]} = CastData,

    {binary_to_atom(Name, utf8), binary_to_atom(Class, utf8), groups(Groups)}. 

casts(Casts) ->
    [cast(Cast) || Cast <- Casts].