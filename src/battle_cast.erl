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

condition({Start, Last, Phase}, CurrSeq) ->
    {CurrSeq + Start, CurrSeq + Start + Last, Phase}.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_cast_effect({Name, Cond, Trans, Prob, React}, {CurrSeq, _, _}, O, D) ->

    Outcome = case rand:uniform() > Prob of
        true -> cast_failed;
        _    -> cast_successful
    end,

    {Name, condition(Cond, CurrSeq), role(Trans, {O, D}), Outcome, React}.

get_effect_list({_Name, List}, S, O, D) ->
    lists:map(fun(Spec) -> parse_cast_effect(Spec, S, O, D) end, List).

get_effect_list_from_name(Name, S, O, D) ->
    get_effect_list(hd(ets:lookup(casts, Name)), S, O, D).



log(casting, CastName, {Seq, Stage, Mover}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, D)},
        { hand, null}, { action, CastName},
        { outcome, cast }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]};

log(casted, {EffectName, Outcome}, {Seq, Stage, Mover}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, D)},
        { hand, null}, { action, EffectName},
        { outcome, Outcome }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.



cast(_S, #{casts:=[]}=O, D, L) ->
    {O#{rem_moves:=0}, D, L};

cast(_S, #{casts:=[null | _]}=O, D, L) ->
    {O#{rem_moves:=0}, D, L};

cast(S, #{id:=OffenderID, casts:=[CastName | RemainingCasts], effects:=Effects}=O, 
        #{id:=DefenderID}=D, L) ->

    CurrEffects = get_effect_list_from_name(CastName, S, OffenderID, DefenderID),
    LogCasting = log(casting, CastName, S, O, D), 
    LogMounting = [log(casted, {Name, Outcome}, S, O, D) || {Name, _, _, Outcome, _} <- CurrEffects],
    NewLog = lists:append(LogMounting, [LogCasting | L]),

    {O#{rem_moves:=0, casts:=RemainingCasts, effects:=lists:append(CurrEffects, Effects)}, D, NewLog}.
