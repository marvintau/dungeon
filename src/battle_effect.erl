
-module(battle_effect).

-author('Yue Marvin Tao').

-export([apply_effect/3]).
% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

set_attr({set, Value}, AttrName, P) -> P#{curr_attr:=#{AttrName=>Value}};
set_attr({add, Incremental}, AttrName, P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=#{AttrName => Original + Incremental}};
set_attr({times, Ratio}, AttrName, P) ->
    Original = get_attr(AttrName, P),
    P#{curr_attr:=#{AttrName => Original * Ratio}}.


set_hp({set, Value}, P) -> P#{hp:=Value};
set_hp({add, Incremental}, #{hp:=Hp}=P) -> P#{hp:=Hp + Incremental};
set_hp({times, Ratio}, #{hp:=Hp}=P) -> P#{hp:=Hp * Ratio}.


% apply_effect: the wrapper function to apply the effects over the player context, and
% mark whether the context is modified. If modified, the function returns {affected, P1, P2},
% otherwise {not_affected, P1, P2}

apply_effect({direct, Op, {role, ToWhat, ToWhom, AttrName}}, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
    case {ToWhat, ToWhom} of
        {to_hp, I1} -> {affected, set_hp(Op, P1), P2};
        {to_hp, I2} -> {affected, P1, set_hp(Op, P2)};
        {to_attr, I1} -> {affected, set_attr(Op, AttrName, P1), P2};
        {to_attr, I2} -> {affected, P1, set_attr(Op, AttrName, P2)}
    end;


apply_effect({indirect, {Op, {role, FromWhat, FromWhom, AttrName}}, To},
            {#{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2}) ->
    case {FromWhat, FromWhom} of
        {from_hp, I1} -> apply_effect({direct, {Op, H1}, To}, {P1, P2});
        {from_hp, I2} -> apply_effect({direct, {Op, H2}, To}, {P1, P2});
        {from_attr, I1} -> apply_effect({direct, {Op, get_attr(AttrName, P1)}, To}, {P1, P2});
        {from_attr, I2} -> apply_effect({direct, {Op, get_attr(AttrName, P1)}, To}, {P1, P2})
    end.


apply_effect(Effect, State, {#{id:=I1}=P1, P2}) ->
    
    {_Name, {Seq, Mover, Phase, Outcome}, Specs} = Effect,

    {CurrSeq, CurrPhase, _, {CurrMover, _, _}, _} = State,

    OutcomeMatches = case Outcome of
        nah -> true;
        _   -> case CurrMover of
            I1 -> Outcome == get_attr(outcome, P1);
            _  -> Outcome == get_attr(outcome, P2)
        end
    end,
   
    case (Phase == CurrPhase) and (OutcomeMatches == true) of
        true -> case {Phase, CurrMover, Mover} of
            {casting, Same, Same}   when (Seq > CurrSeq)   -> apply_effect(Specs, {P1, P2});
            {attacking, Same, Same} when (Seq > CurrSeq)   -> apply_effect(Specs, {P1, P2});
            {settling, I1, _}       when (Seq+1 > CurrSeq) -> apply_effect(Specs, {P1, P2});
            _ -> {not_affected, P1, P2}
        end;
        _    -> {not_affected, P1, P2}
    end.


