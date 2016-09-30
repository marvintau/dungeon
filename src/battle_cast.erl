-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/3]).

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


make_effect({direct, Op, {ToWhat, ToWhom, AttrName}}, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
    case {ToWhat, ToWhom} of
        {to_hp, I1} -> {set_hp(Op, P1), P2};
        {to_hp, I2} -> {P1, set_hp(Op, P2)};
        {to_attr, I1} -> {set_attr(Op, AttrName, P1), P2};
        {to_attr, I2} -> {P1, set_attr(Op, AttrName, P2)}
    end;


make_effect({indirect, {Op, {FromWhat, FromWhom, AttrName}}, To},
            {#{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2}) ->
    case {FromWhat, FromWhom} of
        {from_hp, I1} -> make_effect({direct, {Op, H1}, To}, {P1, P2});
        {from_hp, I2} -> make_effect({direct, {Op, H2}, To}, {P1, P2});
        {from_attr, I1} -> make_effect({direct, {Op, get_attr(AttrName, P1)}, To}, {P1, P2});
        {from_attr, I2} -> make_effect({direct, {Op, get_attr(AttrName, P1)}, To}, {P1, P2})
    end.


make_effect(Effect, State, {#{id:=I1}=P1, P2}) ->

    {_Name, {Seq, Phase, Outcome}, Specs} = Effect,

    {CurrSeq, CurrPhase, _, {Mover, _, _}, _} = State,

    OutcomeMatches = case Outcome of
        nah -> true;
        _   -> case Mover of
            I1 -> Outcome == get_attr(outcome, P1);
            _  -> Outcome == get_attr(outcome, P2)
        end
    end,
    
    case (Seq > CurrSeq) and (Phase == CurrPhase) and (OutcomeMatches == true) of
        true -> make_effect(Specs, {P1, P2});
        _    -> {P1, P2}
    end.

apply_effects({_, _, _, _, []}, P1, P2) -> {P1, P2};
apply_effects(S, P1, P2) ->

    [EffectDescription | Remaining] = element(5, S),
    
    erlang:display(EffectDescription),
    
    {AffectedP1, AffectedP2} = make_effect(EffectDescription, S, {P1, P2}),
    
    apply_effects(setelement(5, S, Remaining), AffectedP1, AffectedP2).

% To set the player finally get modified
assign_role(of_self, {Mover, I1, I2}) -> Mover;
assign_role(of_opponent, {Mover, I1, I2}) when Mover == I1 -> I2;
assign_role(of_opponent, {_, I1, _}) -> I1;

% to create a complete data structure
assign_role({role, What, Whom, Attr}, Movers) -> {role, What, assign_role(Whom, Movers), Attr};

% to create even more complete data structure
assign_role({direct, Op, To}, Movers) -> {direct, Op, assign_role(To, Movers)};
assign_role({indirect, {Op, From}, To}, Movers) -> {indirect, {Op, assign_role(From, Movers)}, assign_role(To, Movers)}.

% to assign the final sequence number
assign_seq({Last, Phase, Outcome}, CurrSeq) -> {CurrSeq + Last, Phase, Outcome}.

% wrap all the operations
update_cast({Name, Cond, Spec}, {CurrSeq, _, _, {Mover, _, _}, _}, I1, I2) ->
    {Name, assign_seq(Cond, CurrSeq), assign_role(Spec, {Mover, I1, I2})}.


get_cast(CastName) -> hd(ets:lookup(casts, CastName)).

% the function that really applies a cast from player's cast table. It
% fetches one cast, update the cast into its final form, and put it into
% the effect list.

cast(S, #{cast_list:=[]}=P1, #{cast_list:=[]}=P2) ->

    {Mover, Hand, _} = element(4, S),

    {no_more_casts, setelement(4, S, {Mover, Hand, 0}), P1, P2};

cast(S, #{id:=I1, cast_list:=Cast1}=P1, #{id:=I2, cast_list:=Cast2}=P2) ->

    {_, _, _, {Mover, Hand, _}, EffectList} = S,

    {CastName, CurrCast1, CurrCast2,  NewCast1, NewCast2} = case Mover of
        I1 -> {hd(Cast1), hd(Cast1), null, tl(Cast1), Cast2};
        _  -> {hd(Cast2), null, hd(Cast2), Cast1, tl(Cast2)}
    end,

    NewEffectList = case CastName of
        null -> EffectList;
        _    -> [update_cast(get_cast(CastName), S, I1, I2) | EffectList]
    end,

    {yet_more_casts, setelement(4, setelement(5, S, NewEffectList), {Mover, Hand, 0}),
     P1#{curr_cast:=CurrCast1, cast_list:=NewCast1}, P2#{curr_cast:=CurrCast2, cast_list:=NewCast2}}.


