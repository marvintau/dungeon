-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/4]).

role(of_self, {Offender, _Defender}) -> Offender;
role(of_opponent, {_Offender, Defender}) -> Defender;

% to create a data structure that holds the role information, including the
% attribute to be modified, and the owner of those attributes.
role({role, What, Whom, Attr}, Movers) -> {role, What, role(Whom, Movers), Attr};

% to create the data structure that holds the operation over the attributes
% and the owner. direct means that the value is assigned to the role directly,
% while indirect means that the value is related to another attribute of some
% role (maybe the initiator or the other one)
role({direct, Op, To}, Movers) -> {direct, Op, role(To, Movers)};
role({indirect, {Op, From}, To}, Movers) -> {indirect, {Op, role(From, Movers)}, role(To, Movers)}.

% to assign the sequence number of terminal condition, and the cast initiator.

% If the cast effects in the current round, then the Start (sequential number
% of starting) should be 0, and the Last (the rounds that the effect of cast
% last for) should be 1.

condition({Start, Last, Phase, Outcome}, CurrSeq) ->
    {CurrSeq + Start, CurrSeq + Start + Last, Phase, Outcome}.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_cast_effect({Name, Cond, Trans, Prob, React}, {CurrSeq, _, _, _, _}, O, D) ->

    Outcome = case rand:uniform() > Prob of
        true -> cast_failed;
        _    -> cast_successful
    end,

    {Name, condition(Cond, CurrSeq), role(Trans, {O, D}), Outcome, React}.

get_effect_list({_Name, List}, S, O, D) ->
    lists:map(fun(Spec) -> parse_cast_effect(Spec, S, O, D) end, List).

get_effect_list_from_name(Name, S, O, D) ->
    get_effect_list(hd(ets:lookup(casts, Name)), S, O, D).



log(casting, CastName, {Seq, Stage, Role, {Mover, _}, _}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { role, Role}, { defender, maps:get(id, D)},
        { hand, null}, { action, CastName}, {rem_atks, null},
        { outcome, cast }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]};

log(casted, {EffectName, Outcome}, {Seq, Stage, Role, {Mover, _}, _}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { role, Role}, { defender, maps:get(id, D)},
        { hand, null}, { action, EffectName}, {rem_atks, null},
        { outcome, Outcome }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.



cast_single(S, #{casts:=[]}=O, D, L) ->
    
    {Mover, _} = element(4, S),
    {setelement(4, S, {Mover, 0}), O, D, L};

cast_single(S, #{casts:=[null | _]}=O, D, L) ->
    {Mover, _} = element(4, S),
    {setelement(4, S, {Mover, 0}), O, D, L};

cast_single(S = {_, _, _,  _, _},
            #{id:=OffenderID, casts:=[CastName | RemainingCasts], effects:=Effects}=O, 
            #{id:=DefenderID}=D, L) ->

    CurrEffects = get_effect_list_from_name(CastName, S, OffenderID, DefenderID),
    LogCasting = log(casting, CastName, S, O, D), 
    LogMounting = [log(casted, {Name, Outcome}, S, O, D) || {Name, _, _, Outcome, _} <- CurrEffects],
    NewLog = lists:append(LogMounting, [LogCasting | L]),

    {S, O#{casts:=RemainingCasts, effects:=lists:append(CurrEffects, Effects)}, D, NewLog}.

cast(S = {_, _, _, {Mover, _}, _}, #{id:=I1}=P1, #{id:=I2}=P2, L) ->

    {CastedState, CastedP1, CastedP2, CastedLog} = case Mover of
        I1 -> cast_single(S, P1, P2, L);
        I2  -> {NewState, NewP2, NewP1, NewLog} = cast_single(S, P2, P1, L), {NewState, NewP1, NewP2, NewLog}
    end,

    {setelement(4, CastedState, {Mover, 0}), CastedP1, CastedP2, CastedLog}.
