-module(battle_cast).

-author('Yue Marvin Tao').

-export([affect/4, apply_effects/3]).

% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

set_attr(AttrName, Value, P) ->
    P#{curr_attr:=#{AttrName=>Value}}.

affect({{value, Value}, {ToWhom, AttrName}}, #{id:=I1}=P1, _P2) when ToWhom==I1->
    set_attr(AttrName, Value, P1);
affect({{value, Value}, {_ToWhom, AttrName}}, _P1, P2) ->
    set_attr(AttrName, Value, P2);

affect({{percentage, Percentage}, {FromWhom, OfAttrName}, {ToWhom, ToAttrName}}, #{id:=I1}=P1, #{id:=I2}=P2) ->
    case {FromWhom, ToWhom} of
        {I1, I1} -> {set_attr(ToAttrName, get_attr(OfAttrName, P1) * Percentage, P1), P2};
        {I1, I2} -> {P1, set_attr(ToAttrName, get_attr(OfAttrName, P1) * Percentage, P2)};
        {I2, I1} -> {set_attr(ToAttrName, get_attr(OfAttrName, P2) * Percentage, P1), P2};
        {I2, I2} -> {P1, set_attr(ToAttrName, get_attr(OfAttrName, P1) * Percentage, P2)}
    end.


affect({Name, Numerics, {Seq, Phase}}, {CurrSeq, CurrPhase, _, _, _}, P1, P2) when (Seq < CurrSeq) and (Phase == CurrPhase) ->

    affect(Numerics, P1, P2).


apply_effects({_, _, _, _, []}, P1, P2) -> {P1, P2};
apply_effects({CurrSeq, CurrPhase, DefOff, MoveType, [EffectDescription | Remaining]}, P1, P2) ->
    
    erlang:display(EffectDescription),
    
    {AffectedP1, AffectedP2} = affect(EffectDescription, P1, P2),
    
    apply_effects({CurrSeq, CurrPhase, DefOff, MoveType, Remaining}, AffectedP1, AffectedP2).
