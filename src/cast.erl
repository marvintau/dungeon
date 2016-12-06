-module(cast).

-author('Yue Marvin Tao').

-export([cast/4]).

-export([apply/2, apply/4]).


rand() -> element(3, erlang:timestamp())/1000000.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_single_effect(Name, {Cond, Trans, EffectNote}, #{seq:=CurrSeq, mover:=Mover}) ->
    {Name, Mover, conds:seq(Cond, CurrSeq), Trans, EffectNote}.

parse_single_group(Name, {Prob, ToWhom, Effects}, S) ->
    case rand() < Prob of
        true -> {success, ToWhom, lists:map(fun(Spec) -> parse_single_effect(Name, Spec, S) end, Effects)};
        _ -> {failed, ToWhom, bad_luck}
    end.

parse_groups(Name, Groups, S) ->
   [parse_single_group(Name, Group, S) || Group <- Groups]. 

log(CastName, ToWhom, failed, #{seq:=Seq, stage:=Stage, mover:=Mover}, O, D) ->

    ToWhomID = case ToWhom of
        off -> maps:get(id, O);
        def -> maps:get(id, D)
    end,

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, ToWhomID},
        { hand, none}, { action, CastName}, {outcome_note, failed},
        { outcome, [] }, { damage, 0 },
        { offender_hp, maps:get(hp, maps:get(state, O)) },
        { defender_hp, maps:get(hp, maps:get(state, D)) }
    ]};

log(_, _, _, _, _, _) -> {[]}.




parse_groups_logged({Name, _Type, Groups}, S, O, D) ->
    Parsed = parse_groups(Name, Groups, S),
    {Logs, Effects} = lists:unzip([{log(Name, ToWhom, Outcome, S, O, D), CurrEffects} || {Outcome, ToWhom, CurrEffects} <- Parsed]),
    {Logs, [Effect || Effect <- lists:flatten(Effects), Effect =/= bad_luck]}.

parse_cast(Name, S, O, D) ->
    parse_groups_logged(hd(ets:lookup(casts, Name)), S, O, D).

cast(_S, #{casts:=[]}=O, D, L) ->
    {O, D, L};

cast(_S, #{casts:=[none | RemainingCasts]}=O, D, L) ->
    {O#{casts:=RemainingCasts}, D, L};

cast(S, #{casts:=[CastName | RemainingCasts], effects:=ExistingEffects}=O, D, L) ->

    {CurrLogs, CurrEffects} = parse_cast(CastName, S, O, D),
    NewEffects = lists:append(CurrEffects, ExistingEffects),
    NewLog = lists:append(CurrLogs, L),

    {O#{casts:=RemainingCasts, effects:=NewEffects}, D, NewLog}.




apply(#{talented:=TalentedP1}=P1, #{talented:=TalentedP2}=P2) ->

    S1 = #{seq=>0, stage=>preparing, mover=>maps:get(id, P1)},
    S2 = #{seq=>0, stage=>preparing, mover=>maps:get(id, P2)},

    {P1CastedLogs, P1Effect} = parse_cast(TalentedP1, S1, P1, P2),
    {P2CastedLogs, P2Effect} = parse_cast(TalentedP2, S2, P2, P1),

    {P1#{effects:=P1Effect}, P2#{effects:=P2Effect}, lists:append(P1CastedLogs, P2CastedLogs)}.


apply(State, #{attr:=#{cast_disabled:=0}}=O, D, Log) ->
    {MovedO, MovedD, MovedLog} = cast(State, O, D, Log),
    effect:apply(State, MovedO#{done:=already}, MovedD, MovedLog);

apply(_State, #{casts:=Casts}=O, D, Log) ->
    ConsumedCasts = case Casts of
        [] -> [];
        [_|RemCasts] -> RemCasts
    end,

    {O#{casts:=ConsumedCasts, done:=already}, D, Log}.
