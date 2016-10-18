-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/4]).

% To set the player finally get modified, according to the indicator in the
% effect specification. for of_self, the role will be assigned as the Mover,
% or the cast initiator, for of_opponent, the opponent of the current Mover.
role(of_self, {Mover, _I1, _I2}) -> Mover;
role(of_opponent, {Mover, I1, I2}) when Mover == I1 -> I2;
role(of_opponent, {_, I1, _}) -> I1;

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

condition({Start, Last, Phase, Outcome}, Mover, CurrSeq) ->
    {CurrSeq + Start, CurrSeq + Start + Last, Mover, Phase, Outcome}.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_cast_effect({Name, Cond, Spec, Prob, React}, {CurrSeq, _, _, {Mover, _}, _}, I1, I2) ->

    Outcome = case rand:uniform() > Prob of
        true -> cast_failed;
        _    -> cast_successful
    end,

    {Name, condition(Cond, Mover, CurrSeq), role(Spec, {Mover, I1, I2}), Outcome, React}.

get_effect_list({_Name, List}, S, I1, I2) ->
    lists:map(fun(Spec) -> parse_cast_effect(Spec, S, I1, I2) end, List).

get_effect_list_from_name(Name, S, I1, I2) ->
    get_effect_list(hd(ets:lookup(casts, Name)), S, I1, I2).



log(cast, CastName, {Seq, Stage, Role, {Mover, _}, _}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { role, Role}, { defender, maps:get(id, D)},
        { hand, null}, { action, CastName}, {rem_atks, null},
        { outcome, cast }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]};

log(casted_effect, {EffectName, Outcome}, {Seq, Stage, Role, {Mover, _}, _}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { role, Role}, { defender, maps:get(id, D)},
        { hand, null}, { action, EffectName}, {rem_atks, null},
        { outcome, Outcome }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.

log_ordered(cast, CastName, S, #{id:=I1}=P1, P2) ->

    {Mover, _} = element(4, S),

    case Mover of
        I1 -> log(cast, CastName, S, P1, P2);
        _  -> log(cast, CastName, S, P2, P1)
    end;

log_ordered(casted_effect, EffectName, S, #{id:=I1}=P1, P2) ->

    {Mover, _} = element(4, S),

    case Mover of
        I1 -> log(casted_effect, EffectName, S, P1, P2);
        _  -> log(casted_effect, EffectName, S, P2, P1)
    end.

cast(S, #{cast_list:=[]}=P1, #{cast_list:=[]}=P2, L) ->

    {Mover, _} = element(4, S),

    {setelement(4, S, {Mover, 0}), P1, P2, L};

cast(S, #{id:=I1, cast_list:=Cast1}=P1, #{id:=I2, cast_list:=Cast2}=P2, L) ->

    {_, _, _, {Mover, _}, EffectList} = S,

    {CastName,  RemCasts1, RemCasts2} = case Mover of
        I1 -> {hd(Cast1), tl(Cast1), Cast2};
        _  -> {hd(Cast2), Cast1, tl(Cast2)}
    end,

    {CurrEffectList, LogCasted} = case CastName of
        null -> {[], L};
        _    -> {get_effect_list_from_name(CastName, S, I1, I2), [log_ordered(cast, CastName, S, P1, P2) | L]}
    end,

    NewEffectList = lists:append(CurrEffectList, EffectList),

    LogMounted = lists:append(lists:map(
        fun({Name, _, _, Outcome, _}) -> log_ordered(casted_effect, {Name, Outcome}, S, P1, P2) end,
        CurrEffectList), LogCasted),

    {setelement(4, setelement(5, S, NewEffectList), {Mover, 0}),
     P1#{cast_list:=RemCasts1}, P2#{cast_list:=RemCasts2}, LogMounted}.
