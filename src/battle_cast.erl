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

parse_single_effect({Name, Cond, Trans, React}, {CurrSeq, _, _}, O, D) ->

    {Name, condition(Cond, CurrSeq), role(Trans, {O, D}), React}.

parse_effects({_Name, Prob, List}, S, O, D) ->
    case rand:uniform() < Prob of
        true -> {success, lists:map(fun(Spec) -> parse_single_effect(Spec, S, O, D) end, List)};
        _ -> {failed, none}
    end.

parse_cast(Name, S, O, D) ->
    parse_effects(hd(ets:lookup(casts, Name)), S, O, D).

log(CastName, Outcome, {Seq, Stage, Mover}, O, D) ->
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, D)},
        { hand, null}, { action, CastName},
        { outcome, Outcome }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.

cast(_S, #{casts:=[]}=O, D, L) ->
    {O#{rem_moves:=0}, D, L};

cast(_S, #{casts:=[null | _]}=O, D, L) ->
    {O#{rem_moves:=0}, D, L};

cast(S, #{id:=Off, casts:=[CastName | RemainingCasts], effects:=ExistingEffects}=O, 
        #{id:=Def}=D, L) ->

    {NewEffects, NewLog} = case parse_cast(CastName, S, Off, Def) of
        {success, CurrEffects} ->
            Log = [log(CastName, success, S, O, D) | L],
            Effects = lists:append(CurrEffects, ExistingEffects),
            {Effects, Log};
        {failed, _} ->
            {ExistingEffects, L}
    end,

    {O#{rem_moves:=0, casts:=RemainingCasts, effects:=NewEffects}, D, NewLog}.
