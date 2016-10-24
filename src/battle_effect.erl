
-module(battle_effect).

-author('Yue Marvin Tao').

-export([effect/4]).
% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

rand_from_interval({Low, High}) ->
    round(Low + rand:uniform() * (High - Low));
rand_from_interval(SingleValue) -> SingleValue.

set_attr({set, Value}, AttrName, #{curr_attr:=Attr}=P) ->
    P#{curr_attr:=Attr#{AttrName:=Value}};

set_attr({add, Inc}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := round(Original + rand_from_interval(Inc))}};

set_attr({times, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    Inc = round(Original * rand_from_interval(Ratio)), 
    P#{curr_attr:=Attr#{AttrName := Original + Inc}};

set_attr({linear, Inc, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := Original + round(rand_from_interval(Inc) * Ratio)}}.

set_hp({set, Value}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    P#{hp:=Value, curr_attr:=Attrs#{damage_taken:=Hp - Value}};

set_hp({add, Inc}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = rand_from_interval(Inc),
    P#{hp:=Hp - FinalInc, 
       curr_attr:=Attrs#{damage_taken:=FinalInc}};

set_hp({times, Ratio}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = round(Hp * rand_from_interval(Ratio)),
    P#{hp:= Hp - FinalInc, curr_attr:=Attrs#{damage_taken:=FinalInc}};

set_hp({linear, Inc, Ratio}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = round(rand_from_interval(Inc) * Ratio),
    P#{hp:=Hp - FinalInc, curr_attr:=Attrs#{damage_taken:=FinalInc}}.

compensate_armor(#{hp:=Hp, curr_attr:=Attrs}=P) ->
    #{armor:=Armor, damage_taken:=Damage} = Attrs,
    NewDamage = Damage * (1 - Armor/10000),
    P#{hp:=Hp + (Damage - NewDamage), curr_attr:=Attrs#{damage_taken:=NewDamage}}.


set_rem_moves({set, Value}, P) ->
    P#{rem_moves:=Value}.


get_player_by_id(Who, #{id:=I1}=P1, #{id:=I2}=P2) ->
    case Who of
        I1 -> P1;
        I2 -> P2
    end.

get_react_outcome(React, ID, P1, P2) ->
    Rand = rand:uniform() * 100,
    
    case React of
        resistable -> case (Rand > get_attr(resist, get_player_by_id(ID, P1, P2))) of
            true -> {ok, none};
            _ -> {not_affected, resisted}
        end;

        absorbable -> {affected, absorbed};
        none -> {ok, none}
    end.

check_condition({StartingSeq, TerminalSeq, Phase}, CurrSeq, CurrPhase) ->
    (CurrSeq >= StartingSeq) and (CurrSeq < TerminalSeq) and (Phase == CurrPhase).
check_condition(EffectCond, {CurrSeq, CurrPhase, _}) ->
    check_condition(EffectCond, CurrSeq, CurrPhase).

% apply_effect: the wrapper function to apply the effects over the player context, and
% mark whether the context is modified. If modified, the function returns {affected, P1, P2},
% otherwise {not_affected, P1, P2}

apply_effect({direct, Op, {role, to_hp, ToWhom, _}}, React, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
   
    FinalReact = get_react_outcome(React, ToWhom, P1, P2),

    case {FinalReact, ToWhom} of
        {{not_affected, Reaction}, _} -> {{not_affected, Reaction}, P1, P2};

        {{affected, absorbed}, I1} ->
            {affected, compensate_armor(set_hp(Op, P1)), P2};

        {{affected, absorbed}, I2} ->
            {affected, P1, compensate_armor(set_hp(Op, P2))};

        {_, I1} -> {affected, set_hp(Op, P1), P2};
        {_, I2} -> {affected, P1, set_hp(Op, P2)}
    end;

apply_effect({direct, Op, {role, to_attr, ToWhom, AttrName}}, React, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
    
    FinalReact = get_react_outcome(React, ToWhom, P1, P2),

    case {FinalReact, ToWhom} of
        {{not_affected, Reaction}, _} -> {{not_affected, Reaction}, P1, P2};
        {_, I1} -> {affected, set_attr(Op, AttrName, P1), P2};
        {_, I2} -> {affected, P1, set_attr(Op, AttrName, P2)}
    end;

apply_effect({direct, Op, {role, to_rem_moves, ToWhom, _}}, React, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
    
    FinalReact = get_react_outcome(React, ToWhom, P1, P2),

    case {FinalReact, ToWhom} of
        {{not_affected, Reaction}, _} -> {{not_affected, Reaction}, P1, P2};
        {_, I1} -> {affected, set_rem_moves(Op, P1), P2};
        {_, I2} -> {affected, P1, set_rem_moves(Op, P2)}
    end;


apply_effect({indirect, {linear, {role, FromWhat, FromWhom, AttrName}, Ratio}, To}, React,
            {#{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2}) ->
    case {FromWhat, FromWhom} of
        {from_hp, I1} -> apply_effect({direct, {linear, H1, Ratio}, To}, React, {P1, P2});
        {from_hp, I2} -> apply_effect({direct, {linear, H2, Ratio}, To}, React, {P1, P2});
        {from_attr, I1} -> apply_effect({direct, {linear, get_attr(AttrName, P1), Ratio}, To}, React, {P1, P2});
        {from_attr, I2} -> apply_effect({direct, {linear, get_attr(AttrName, P1), Ratio}, To}, React, {P1, P2})
    end;


apply_effect(Effect, State, {O, D}) ->

    {_Name, EffectCond = {_, _, _Phase}, Specs, React} = Effect,

    case check_condition(EffectCond, State) of
        
        true -> apply_effect(Specs, React, {O, D});
        
        _    -> {{not_affected, not_correct_cond}, O, D}
    end.


log({Seq, Stage, Mover}, {EffectName, _, _, _}, ReactOutcome, O, D) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, D)},

        { hand, null}, { action, EffectName},
        { outcome, ReactOutcome }, { damage, maps:get(damage_taken, maps:get(curr_attr, D)) },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.


% When applying effects, the effect list doesn't change. For every turn,
% we just check whether the effect description meets the condition, and
% apply. Hence here we only change the player context.


effect(S, #{effects:=Effects}=O, D, Log) ->
    effect(S, O, D, Log, Effects).

effect(_S, O, D, Log, []) ->
    {O, D, Log};

effect(S, O, D, Log, [EffectSpec| Remaining]) ->

    {EffectedOffender, EffectedDefender, NewLog} =
    case apply_effect(EffectSpec, S, {O, D}) of
        {affected, AffectedP1, AffectedP2} ->
            NextLog = [log(S, EffectSpec, effect, AffectedP1, AffectedP2) | Log],
            {AffectedP1, AffectedP2, NextLog};
        {{not_affected, React}, NotAffectedP1, NotAffectedP2} when (React==resist) or (React==block) ->
            NextLog = [log(S, EffectSpec, React, NotAffectedP1, NotAffectedP2) | Log],
            {NotAffectedP1, NotAffectedP2, NextLog};
        {{not_affected, _}, NotAffectedP1, NotAffectedP2} ->
            {NotAffectedP1, NotAffectedP2, Log}

    end,
     
    effect(S, EffectedOffender, EffectedDefender, NewLog, Remaining).

