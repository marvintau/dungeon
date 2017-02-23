-module(cast).

-author('Yue Marvin Tao').

-export([apply/4]).
-export([apply_opening/4]).


rand() -> element(3, erlang:timestamp())/1000000.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_single_effect(Name, {Cond, Trans}, #{seq:=CurrSeq, mover:=Mover}) ->
    {Name, Mover, conds:seq(Cond, CurrSeq), Trans}.

parse_single_group(Name, {Prob, Effects}, S) ->
    case rand() < Prob of
        true -> lists:map(fun(Spec) -> parse_single_effect(Name, Spec, S) end, Effects);
        _ ->    bad_luck
    end.

parse_groups(Name, Groups, S) ->
   [parse_single_group(Name, Group, S) || Group <- Groups]. 

log(CastName, CurrEffect, #{seq:=Seq, stage:=Stage, mover:=Mover}, #{state:=#{position:=PosO}}=O, #{state:=#{position:=PosD}}=D) ->

    Damage = case CurrEffect of
        bad_luck -> 4294409;
        _ -> 0
    end,

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { action, CastName},
        { effects, [] }, { damage, Damage },
        { offenderHP, maps:get(hp, maps:get(state, O)) },
        { defenderHP, maps:get(hp, maps:get(state, D)) },
        { offenderPos, PosO}, {defenderPos, PosD},
        { offenderPosAct, none}, {defenderPosAct, none}
    ]}.


parse_groups_logged({Name, _Type, Groups}, S, O, D) ->
    Parsed = parse_groups(Name, Groups, S),
    {Logs, Effects} = lists:unzip([{log(Name, CurrEffect, S, O, D), CurrEffect} || CurrEffect <- Parsed]),
    {Logs, [Effect || Effect <- lists:flatten(Effects), Effect =/= bad_luck]}.

parse_cast(Name, S, O, D) ->
    parse_groups_logged(hd(ets:lookup(casts, Name)), S, O, D).


cast(_S, #{casts:=[]}=O, D, L) ->
    {O, D, L};

cast(_S, #{casts:=[none | RemainingCasts]}=O, D, L) ->
    {O#{casts:=RemainingCasts}, D, L};

cast(S, #{attr:=#{cast_disabled:=CastDisabled}, casts:=[CastName | RemainingCasts], effects:=ExistingEffects}=O, D, L) ->

    {NextEffects, NextLog} = case CastDisabled of 
    0 ->
        {CurrLogs, CurrEffects} = parse_cast(CastName, S, O, D),
        NewEffects = lists:append(CurrEffects, ExistingEffects),
        NewLog = lists:append(CurrLogs, L),
        {NewEffects, NewLog};
    _ ->
        {ExistingEffects, L}
    end,

    {O#{casts:=RemainingCasts, effects:=NextEffects}, D, NextLog}.


apply(State, O, D, Log) ->
    {CastedO, CastedD, CastedLog} = cast(State, O, D, Log),
    {MovedO, MovedD, MovedLog} = effect:apply(State, CastedO, CastedD, CastedLog),
    {MovedO#{done:=already}, MovedD, MovedLog}.


cast_opening(_S, #{talented:=none}=O, D, L) ->
    {O, D, L};

cast_opening(S, #{attr:=#{cast_disabled:=CastDisabled}, talented:=Opening, effects:=ExistingEffects}=O, D, L) ->

    {NextEffects, NextLog} = case CastDisabled of
    0 ->
        {CurrLogs, CurrEffects} = parse_cast(Opening, S, O, D),
        NewEffects = lists:append(CurrEffects, ExistingEffects),
        NewLog = lists:append(CurrLogs, L),
        {NewEffects, NewLog};
    _ ->
        {ExistingEffects, L}
    end,

    {O#{effects:=NextEffects}, D, NextLog}.

apply_opening(State, O, D, Log) ->
    {MovedO, MovedD, MovedLog} = cast_opening(State, O, D, Log),
    {EffectedO, EffectedD, EffectedLog} = effect:apply(State, MovedO, MovedD, MovedLog),
    {EffectedO#{done:=already}, EffectedD, EffectedLog}.
