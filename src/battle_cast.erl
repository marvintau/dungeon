-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/4]).

-export([cast_talented/2, cast_effected/4]).


rand() -> element(3, erlang:timestamp())/1000000.

% to assign the sequence number of terminal condition, and the cast initiator.

% If the cast effects in the current round, then the Start (sequential number
% of starting) should be 0, and the Last (the rounds that the effect of cast
% last for) should be 1.


condition({{Start, null, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, 9999, Phase}, Others};

condition({{Start, Last, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, CurrSeq + Start + Last, Phase}, Others}.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_single_effect(Name, {Cond, Trans}, #{seq:=CurrSeq, mover:=Mover}) ->
    {Name, Mover, condition(Cond, CurrSeq), Trans}.

parse_single_group(Name, {Prob, ToWhom, Effects}, S) ->
    case rand() < Prob of
        true -> {success, ToWhom, lists:map(fun(Spec) -> parse_single_effect(Name, Spec, S) end, Effects)};
        _ -> {failed, ToWhom, bad_luck}
    end.

parse_groups(Name, Groups, S) ->
   [parse_single_group(Name, Group, S) || Group <- Groups]. 

log(CastName, ToWhom, Outcome, #{seq:=Seq, stage:=Stage, mover:=Mover}, O, D) ->

    ToWhomID = case ToWhom of
        off -> maps:get(id, O);
        def -> maps:get(id, D)
    end,

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, ToWhomID},
        { hand, none}, { action, CastName},
        { outcome, Outcome }, { damage, 0 },
        { offender_hp, maps:get(hp, maps:get(state, O)) },
        { defender_hp, maps:get(hp, maps:get(state, D)) }
    ]}.

parse_groups_logged({Name, _Type, Groups}, S, O, D) ->
    Parsed = parse_groups(Name, Groups, S),
    {Logs, Effects} = lists:unzip([{log(Name, ToWhom, Outcome, S, O, D), CurrEffects} || {Outcome, ToWhom, CurrEffects} <- Parsed]),

    {Logs, [Effect || Effect <- lists:flatten(Effects), Effect =/=bad_luck]}.

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

cast_talented(#{talented:=TalentedP1}=P1, #{talented:=TalentedP2}=P2) ->

    S1 = #{seq=>0, stage=>preparing, mover=>maps:get(id, P1)},
    S2 = #{seq=>0, stage=>preparing, mover=>maps:get(id, P2)},

    {P1CastedLogs, P1Effect} = parse_cast(TalentedP1, S1, P1, P2),
    {P2CastedLogs, P2Effect} = parse_cast(TalentedP2, S2, P2, P1),

    {P1#{effects:=P1Effect}, P2#{effects:=P2Effect}, lists:append(P1CastedLogs, P2CastedLogs)}.


cast_effected(State, #{attr:=#{cast_disabled:=0}}=O, D, Log) ->
    {MovedO, MovedD, MovedLog} = battle_cast:cast(State, O, D, Log),
    battle_effect:effect(State, MovedO#{done:=already}, MovedD, MovedLog);

cast_effected(_State, #{casts:=Casts}=O, D, Log) ->
    ConsumedCasts = case Casts of
        [] -> [];
        [_|RemCasts] -> RemCasts
    end,

    {O#{casts:=ConsumedCasts, done:=already}, D, Log}.
