-module(casts_to_erlang).

-author('Yue Marvin Tao').

-export([casts/1]).

ref({[{<<"attr_type">>, Type}, {<<"attr">>, Attribute}, {<<"role">>, Role}]}) ->
    {binary_to_atom(Type, utf8), binary_to_atom(Attribute, utf8), binary_to_atom(Role, utf8) };
ref({[{<<"min">>, Min}, {<<"max">>, Max}]}) ->
    {Min, Max};
ref({[{<<"value">>, Value}]}) ->
    Value.

operand({[{<<"inc">>, Inc}, {<<"mul">>, Mul}]}) -> {ref(Inc), ref(Mul)};
operand({[{<<"inc">>, Inc}]}) -> ref(Inc).

operator({[{<<"opcode">>, OpCode}, {<<"operand">>, Operand}, {<<"note">>, Note}]}) ->
    {binary_to_atom(OpCode, utf8), operand(Operand), binary_to_atom(Note, utf8) }.

trans({[{<<"operator">>, Operator}, {<<"to_whom">>, ToWhom}]}) ->
    {operator(Operator), ref(ToWhom)}.

trans_list(TransList) ->
    [trans(Trans) || Trans <- TransList].

comp_cond({[{<<"value">>, Value}, {<<"op">>, Op}, {<<"ref">>, Ref}]}) ->

    ParsedValue = case is_number(Value) of
        true -> Value;
        _ -> binary_to_atom(Value, utf8)
    end,

    {ParsedValue, binary_to_atom(Op, utf8), ref(Ref)}.

comp_cond_list(CompCondList) ->
    [comp_cond(CompCond) || CompCond <- CompCondList].

seq_cond({[{<<"start">>, Start}, {<<"last">>, Last}, {<<"stage">>, Stage}]}) ->
    {Start, Last, binary_to_atom(Stage, utf8)}.

conds({[{<<"seq_cond">>, SeqCond}, {<<"comp_cond_list">>, CompCondList}]}) ->
    {seq_cond(SeqCond), comp_cond_list(CompCondList)}.

single_effect({[{<<"conds">>, Conds}, {<<"trans_list">>, TransList}]}) ->
    {conds(Conds), trans_list(TransList)}.

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
