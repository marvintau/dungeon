-module(casts_to_json).

-author('Yue Marvin Tao').

-export([casts/1, all_names/0, names_with/1]).

parse_role_to_json({role, What, Whom, Attr}) ->
    {[{what, What}, {whom, Whom}, {attr, Attr}]}.

parse_range_to_json({Min, Max}) ->
    {[{type, range}, {value, {[{min, Min}, {max, Max}]} } ]};
parse_range_to_json(Value) ->
    {[{type, value}, {value, Value}]}.


parse_op_to_json({linear, {role, FromWhat, FromWhom, FromAttr}, Ratio}) ->
    {[{type, linear}, {ratio, Ratio}, {from, parse_role_to_json({role, FromWhat, FromWhom, FromAttr})}]};

parse_op_to_json({linear, IncRange, Ratio}) ->
    {[{type, linear}, {ratio, Ratio}, {from, parse_range_to_json(IncRange)}]};

parse_op_to_json({Op, IncRange}) ->
    {[{type, Op}, {from, parse_range_to_json(IncRange)}]}.

parse_trans_to_json({TransType, Op, To}) ->
    {[{type, TransType}, {op, parse_op_to_json(Op)}, {to, parse_role_to_json(To)}]}.

parse_single_effect_to_json(Effect) ->
    {Name, {Start, LastFor, Stage}, Trans, PossibleReact} = Effect,

    RoundCondition = {[{start, Start}, {last_for, LastFor}, {stage, Stage}]},

    {[{name, Name}, {round_cond, RoundCondition}, {trans, parse_trans_to_json(Trans)}, {react, PossibleReact}]}.

parse_single_group_to_json({Prob, Effects}) ->
    {[{prob, Prob}, {effects, [parse_single_effect_to_json(Effect) || Effect <- Effects]}]}.

parse_single_cast_to_json(Cast) ->
    {Name, Class, Groups} = Cast,

    {[{name, Name}, {class, Class}, {groups, [parse_single_group_to_json(Group) || Group <- Groups]}]}.

parse_casts_to_json(Casts) ->
    jiffy:encode([parse_single_cast_to_json(Cast) || Cast <- Casts]).

all_names() ->
    AllCasts = lists:flatten(ets:match(casts, '$1')), 
    {done, parse_casts_to_json(AllCasts)}.

names_with(Class) ->
    Talented = lists:flatten(ets:match(casts, {'$1', talent, '_'})),
    General = lists:flatten(ets:match(casts, {'$1', general, '_'})),
    ClassCast = lists:flatten(ets:match(casts, {'$1', Class, '_'})),
    lists:append([Talented, ClassCast, General]).

casts(Data) ->
    {[{<<"id">>, _ID}, {<<"class">>, Class}]} = Data,
    ReturnedData = [ none | names_with(binary_to_atom(Class, utf8))],    
    error_logger:info_report(ReturnedData),
    {done, jiffy:encode(ReturnedData)}.
