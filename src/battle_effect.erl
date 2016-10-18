
-module(battle_effect).

-author('Yue Marvin Tao').

-export([apply_effects/4]).
% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

set_attr({set, Value}, AttrName, #{curr_attr:=Attr}=P) ->
    P#{curr_attr:=Attr#{AttrName:=Value}};

set_attr({add, {Low, High}}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    Incremental = round(Low + rand:uniform() * (High - Low)),
    P#{curr_attr:=Attr#{AttrName := Original + Incremental}};
set_attr({add, Incremental}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := Original + Incremental}};

set_attr({times, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := round(Original * Ratio)}};

set_attr({linear, {Low, High}, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    Incremental = round(Low + rand:uniform() * (High - Low)),
    P#{curr_attr:=Attr#{AttrName := Original + round(Incremental * Ratio)}};
set_attr({linear, Incremental, Ratio}, AttrName, #{curr_attr:=Attr}=P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=Attr#{AttrName := Original + round(Incremental * Ratio)}}.

set_hp({set, Value}, P) -> P#{hp:=Value};

set_hp({add, {Low, High}}, #{hp:=Hp}=P) ->
    P#{hp:=Hp + round(Low + rand:uniform() * (High - Low))};
set_hp({add, Incremental}, #{hp:=Hp}=P) ->
    P#{hp:=Hp + Incremental};

set_hp({times, {Low, High}}, #{hp:=Hp}=P) ->
    P#{hp:=round(Hp * (Low + rand:uniform() * (High - Low)))};
set_hp({times, Ratio}, #{hp:=Hp}=P) ->
    P#{hp:= round(Hp * Ratio)};

set_hp({linear, {Low, High}, Ratio}, #{hp:=Hp}=P) -> 
    Incremental = round(Low + rand:uniform() + (High - Low)),
    P#{hp:=Hp + round(Incremental * Ratio)};
set_hp({linear, Incremental, Ratio}, #{hp:=Hp}=P) -> P#{hp:=Hp + round(Incremental * Ratio)}.


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

check_condition({StartingSeq, TerminalSeq, Mover, Phase, _}, CurrSeq, CurrPhase, CurrMover, OutcomeMatches) ->
    (CurrSeq >= StartingSeq) and (CurrSeq < TerminalSeq) and (Mover == CurrMover) and (Phase == CurrPhase) and OutcomeMatches.
check_condition(EffectCond, {CurrSeq, CurrPhase, _, {CurrMover, _}, _}, OutcomeMatches) ->
    check_condition(EffectCond, CurrSeq, CurrPhase, CurrMover, OutcomeMatches).

% apply_effect: the wrapper function to apply the effects over the player context, and
% mark whether the context is modified. If modified, the function returns {affected, P1, P2},
% otherwise {not_affected, P1, P2}

apply_effect({direct, Op, {role, to_hp, ToWhom, _}}, React, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
   
    FinalReact = get_react_outcome(React, ToWhom, P1, P2),

    case {FinalReact, ToWhom} of
        {{not_affected, Reaction}, _} -> {{not_affected, Reaction}, P1, P2};

        {{affected, absorbed}, I1} ->
            #{hp:=Hp} = NewP1 = set_hp(Op, P1),
            {affected, NewP1#{hp:=round(Hp * (1 - get_attr(armor, P1)/10000))}, P2};

        {{affected, absorbed}, I2} ->
            #{hp:=Hp} = NewP2 = set_hp(Op, P2),
            {affected, P1, NewP2#{hp:=round(Hp * (1 - get_attr(armor, P2)/10000))}};

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


apply_effect(Effect, State, {#{id:=I1}=P1, P2}) ->

    {_Name, EffectCond = {_, _, _, Phase, Outcome}, Specs, ProbOutcome, React} = Effect,

    {_CurrSeq, _CurrPhase, _, {CurrMover, _}, _} = State,

    OutcomeMatches = case Outcome of
        nah -> true;
        _   -> case CurrMover of
            I1 -> Outcome == get_attr(outcome, P1);
            _  -> Outcome == get_attr(outcome, P2)
        end
    end,

    case check_condition(EffectCond, State, OutcomeMatches) and (ProbOutcome == cast_successful) of
        true -> case Phase of
            
            settling when CurrMover == I1 ->
                apply_effect(Specs, React, {P1, P2});

            _ ->
                apply_effect(Specs, React, {P1, P2})
            
        end;
        
        _    -> {{not_affected, not_correct_cond}, P1, P2}
    end.


log({Seq, Stage, Role, {Mover, _}, _}, {EffectName, _, _, _, _}, ReactOutcome, #{id:=I1} = P1, P2) ->

    {O, D} = case Mover of
        I1 -> {P1, P2};
        _  -> {P2, P1}
    end,

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

apply_effects({_, _, _, _, []}, P1, P2, Log) -> {P1, P2, Log};
apply_effects(S, P1, P2, Log) ->

    [EffectDescription | Remaining] = element(5, S),
   
    {NewP1, NewP2, NewLog} =
    case apply_effect(EffectDescription, S, {P1, P2}) of
        {affected, AffectedP1, AffectedP2} ->
            NextLog = [log(S, EffectDescription, effect, AffectedP1, AffectedP2) | Log],
            {AffectedP1, AffectedP2, NextLog};
        {{not_affected, React}, NotAffectedP1, NotAffectedP2} when (React==resist) or (React==block) ->
            NextLog = [log(S, EffectDescription, React, NotAffectedP1, NotAffectedP2) | Log],
            {NotAffectedP1, NotAffectedP2, NextLog};
        {{not_affected, _}, NotAffectedP1, NotAffectedP2} ->
            {NotAffectedP1, NotAffectedP2, Log}

    end,
    
    apply_effects(setelement(5, S, Remaining), NewP1, NewP2, NewLog).


