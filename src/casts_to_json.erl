-module(casts_to_json).

-author('Yue Marvin Tao').

-export([all_casts/0, casts/1, all_names/0, names_with/1]).

ref({Type, Attribute, Role}) ->
    {[{attr_type, Type}, {attr, Attribute}, {role, Role}]};
ref({Min, Max}) ->
    {[{min, Min}, {max, Max}]};
ref(Value) ->
    {[{value, Value}]}.

operand({Inc, Mul}) -> {[{inc, ref(Inc)}, {mul, ref(Mul)}]};
operand(Inc) -> {[{inc, ref(Inc)}]}.

operator({Opcode, Operand, Note}) ->
    {[{opcode, Opcode}, {operand, operand(Operand)}, {note, Note}]}.

trans({Operator, ToWhom}) ->
    {[{operator, operator(Operator)}, {to_whom, ref(ToWhom)}]}.

trans_list(TransList) ->
    [trans(Trans) || Trans <- TransList].

seq_cond({Start, Last, Stage}) ->
    {[{start, Start}, {last, Last}, {stage, Stage}]}.

comp_cond({Value, Opcode, Ref}) ->
    {[{value, Value}, {op, Opcode}, {ref, ref(Ref)}]}.

comp_cond_list(CompCondList) ->
    [comp_cond(CompCond) || CompCond <- CompCondList].

conds({Seq, CompCondList}) ->
    {[{seq_cond, seq_cond(Seq)}, {comp_cond_list, comp_cond_list(CompCondList)}]}.

effect({Conds, TransList}) ->
    {[{conds, conds(Conds)}, {trans_list, trans_list(TransList)}]}.

effect_list(EffectList) ->
    [effect(Effect) || Effect <- EffectList].

effect_prob_group({Prob, EffectList}) ->
    {[{prob, Prob}, {effects, effect_list(EffectList)}]}.

effect_prob_group_list(EffectProbGroupList) ->
    [effect_prob_group(EffectProbGroup) || EffectProbGroup <- EffectProbGroupList].

cast({Name, Class, EffectProbGroupsList}) ->
    Cast = {[{name, Name}, {class, Class}, {effect_prob_group_list, effect_prob_group_list(EffectProbGroupsList)}]},
    Cast.

all_casts() ->
    AllCasts = lists:flatten(ets:match(casts, '$1')),
    {done, jiffy:encode([cast(Cast) || Cast <- AllCasts])}.

all_names() ->
    AllCasts = lists:flatten(ets:match(casts, '$1')), 
    {done, casts(AllCasts)}.

names_with(Class) ->
    General = lists:flatten(ets:match(casts, {'$1', general, '_'})),
    ClassCast = lists:flatten(ets:match(casts, {'$1', Class, '_'})),
    lists:append([ClassCast, General]).

casts(Data) ->
    {[{<<"id">>, _ID}, {<<"class">>, Class}]} = Data,
    ReturnedData = [ none | names_with(binary_to_atom(Class, utf8))],    
    error_logger:info_report(ReturnedData),
    {done, jiffy:encode(ReturnedData)}.
