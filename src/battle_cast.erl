-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/3]).



% To set the player finally get modified, according to the indicator in the
% effect specification. for of_self, the role will be assigned as the Mover,
% or the cast initiator, for of_opponent, the opponent of the current Mover.
assign_role(of_self, {Mover, _I1, _I2}) -> Mover;
assign_role(of_opponent, {Mover, I1, I2}) when Mover == I1 -> I2;
assign_role(of_opponent, {_, I1, _}) -> I1;

% to create a data structure that holds the role information, including the
% attribute to be modified, and the owner of those attributes.
assign_role({role, What, Whom, Attr}, Movers) -> {role, What, assign_role(Whom, Movers), Attr};

% to create the data structure that holds the operation over the attributes
% and the owner. direct means that the value is assigned to the role directly,
% while indirect means that the value is related to another attribute of some
% role (maybe the initiator or the other one)
assign_role({direct, Op, To}, Movers) -> {direct, Op, assign_role(To, Movers)};
assign_role({indirect, {Op, From}, To}, Movers) -> {indirect, {Op, assign_role(From, Movers)}, assign_role(To, Movers)}.


% to assign the sequence number of terminal condition, and the cast initiator.
assign_cond({Last, Phase, Outcome}, Mover, CurrSeq) -> {CurrSeq + Last, Mover, Phase, Outcome}.




% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

get_effect({Name, Cond, Spec}, {CurrSeq, _, _, {Mover, _, _}, _}, I1, I2) ->
    {Name, assign_cond(Cond, Mover, CurrSeq), assign_role(Spec, {Mover, I1, I2})}.

get_effect_list({_Name, EffectList}, State, I1, I2) ->
    lists:map(fun(EffectSpec) -> get_effect(EffectSpec, State, I1, I2) end, EffectList).

get_single_cast(CastName, State, PlayerID1, PlayerID2) -> get_effect_list(hd(ets:lookup(casts, CastName)), State, PlayerID1, PlayerID2).





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
        _    -> lists:append(get_single_cast(CastName, S, I1, I2), EffectList)
    end,

    {yet_more_casts, setelement(4, setelement(5, S, NewEffectList), {Mover, Hand, 0}),
     P1#{curr_cast:=CurrCast1, cast_list:=NewCast1}, P2#{curr_cast:=CurrCast2, cast_list:=NewCast2}}.


