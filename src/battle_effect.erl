
-module(battle_effect).

-author('Yue Marvin Tao').

-export([apply_effects/4]).
% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

rand_from_interval({Low, High}) ->
    Low + rand:uniform() * (High - Low);
rand_from_interval(SingleValue) -> SingleValue.

set_attr({set, Value}, AttrName, #{curr_attr:=Attr}=P) ->
    P#{curr_attr:=Attr#{AttrName:=Value}};

set_attr({add, Incremental}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := round(Original + rand_from_interval(Incremental))}};

set_attr({times, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := round(Original * rand_from_interval(Ratio))}};

set_attr({linear, Incremental, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := Original + round(rand_from_interval(Incremental) * Ratio)}}.

set_hp({set, Value}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    P#{hp:=Value, curr_attr:=Attrs#{damage_taken:=Hp - Value}};

set_hp({add, Incremental}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = rand_from_interval(Incremental),
    P#{hp:=Hp + FinalInc, 
       curr_attr:=Attrs#{damage_taken:=FinalInc}};

set_hp({times, Ratio}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = round(Hp * rand_from_interval(Ratio)),
    P#{hp:= Hp - FinalInc, curr_attr:=Attrs#{damage_taken:=FinalInc}};

set_hp({linear, Incremental, Ratio}, #{hp:=Hp, curr_attr:=Attrs}=P) ->
    FinalInc = round(rand_from_interval(Incremental) * Ratio),
    P#{hp:=Hp - FinalInc, curr_attr:=Attrs#{damage_taken:=FinalInc}}.

compensate_armor(#{hp:=Hp, curr_attr:=Attrs}=P) ->
    #{armor:=Armor, damage_taken:=Damage} = Attrs,
    NewDamage = Damage * (1 - Armor/10000),
    P#{hp:=Hp + (Damage - NewDamage), curr_attr:=Attrs#{damage_taken:=NewDamage}}.

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

check_condition({StartingSeq, TerminalSeq, Phase, _}, CurrSeq, CurrPhase, OutcomeMatches) ->
    (CurrSeq >= StartingSeq) and (CurrSeq < TerminalSeq) and (Phase == CurrPhase) and OutcomeMatches.
check_condition(EffectCond, {CurrSeq, CurrPhase, _, _}, OutcomeMatches) ->
    check_condition(EffectCond, CurrSeq, CurrPhase, OutcomeMatches).

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


apply_effect({indirect, {Op, {role, FromWhat, FromWhom, AttrName}}, To}, React,
            {#{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2}) ->
    case {FromWhat, FromWhom} of
        {from_hp, I1} -> apply_effect({direct, {Op, H1}, To}, React, {P1, P2});
        {from_hp, I2} -> apply_effect({direct, {Op, H2}, To}, React, {P1, P2});
        {from_attr, I1} -> apply_effect({direct, {Op, get_attr(AttrName, P1)}, To}, React, {P1, P2});
        {from_attr, I2} -> apply_effect({direct, {Op, get_attr(AttrName, P1)}, To}, React, {P1, P2})
    end;


apply_effect(Effect, State, {O, D}) ->

    {_Name, EffectCond = {_, _, _Phase, Outcome}, Specs, ProbOutcome, React} = Effect,

    OutcomeMatches = case Outcome of
        nah -> true;
        _   -> Outcome == get_attr(outcome, O)
    end,

    case check_condition(EffectCond, State, OutcomeMatches) and (ProbOutcome == cast_successful) of
        
        true -> apply_effect(Specs, React, {O, D});
        
        _    -> {{not_affected, not_correct_cond}, O, D}
    end.


log({Seq, Stage, Role, {Mover, _}}, {EffectName, _, _, _, _}, ReactOutcome, O, D) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, {role, Role}, { defender, maps:get(id, D)},

        { hand, null}, { action, EffectName}, {rem_atks, null},
        { outcome, ReactOutcome }, { damage, null },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.


% When applying effects, the effect list doesn't change. For every turn,
% we just check whether the effect description meets the condition, and
% apply. Hence here we only change the player context.

effect_single(_S, #{effects:=[]}=O, D, Log) ->
    {O, D, Log};
effect_single(S, #{effects:=[EffectSpec | Remaining]} = O, D, Log) ->

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
     
    effect_single(S, EffectedOffender#{effects:=Remaining}, EffectedDefender, NewLog).


apply_effects(S = {_, _, _, {Mover, _}},
              #{id:=I1, effects:=OrigEffects1}=P1,
              #{id:=I2, effects:=OrigEffects2}=P2, Log) ->

    {EffectedP1, EffectedP2, EffectedLog} = case Mover of
        I1 -> effect_single(S, P1, P2, Log);
        I2  -> {NewP2, NewP1, NewLog} = effect_single(S, P2, P1, Log), {NewP1, NewP2, NewLog}
    end,

    {EffectedP1#{effects:=OrigEffects1},
     EffectedP2#{effects:=OrigEffects2}, EffectedLog}.
