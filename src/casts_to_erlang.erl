-module(casts_to_erlang).

-author('Yue Marvin Tao').

-export([cast/1]).

range({[{_, <<"range">>}, {_, {[{_, Min}, {_, Max}]} } ]}) ->
    {Min, Max};

range({[_, {_, Value}]}) ->
    if 
        is_number(Value) or is_boolean(Value) -> Value;
        is_binary(Value) -> binary_to_atom(Value, utf8)
    end.

role({[{_, What}, {_, Whom}, {_, Attr}]}) ->
    {role, binary_to_atom(What, utf8), binary_to_atom(Whom, utf8), binary_to_atom(Attr, utf8)}.

op(direct, {[{_, Type}, {_, From}]}) ->
    {binary_to_atom(Type, utf8), range(From)};
op(indirect, {[{_, Type}, {_, Ratio}, {_, Role}]}) ->
    {binary_to_atom(Type, utf8), Ratio, role(Role)}.

trans({[{_, <<"direct">>}, {_, Op}, {_, To}]}) ->
    {direct, op(direct, Op), role(To)};
trans({[{_, <<"indirect">>}, {_, Op}, {_, To}]}) ->
    {indirect, op(indirect, Op), role(To)}.

condition({[{_, Start}, {_, Last}, {_, Stage}]}) ->
    {Start, Last, binary_to_atom(Stage, utf8)}.

single_effect({[{_, Name}, {_, Cond}, {_, Trans}, {_, React}]}) ->
    {binary_to_atom(Name, utf8), condition(Cond), trans(Trans), binary_to_atom(React, utf8)}.

effects(Effects) ->
    [single_effect(Effect) || Effect <- Effects].

single_group({[{_, Prob}, {_, Effects}]}) ->
    {Prob, effects(Effects)}.

groups(Groups) ->
   [single_group(Group) || Group <- Groups]. 

cast(CastData) ->
    {[{_, Name}, {_, Class}, {_, Groups}]} = CastData,

    {binary_to_atom(Name, utf8), binary_to_atom(Class, utf8), groups(Groups)}. 