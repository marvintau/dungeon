-module(battle_cast).

-author('Yue Marvin Tao').

-export([cast/3]).



% To set the player finally get modified
assign_role(of_self, {Mover, _I1, _I2}) -> Mover;
assign_role(of_opponent, {Mover, I1, I2}) when Mover == I1 -> I2;
assign_role(of_opponent, {_, I1, _}) -> I1;

% to create a complete data structure
assign_role({role, What, Whom, Attr}, Movers) -> {role, What, assign_role(Whom, Movers), Attr};

% to create even more complete data structure
assign_role({direct, Op, To}, Movers) -> {direct, Op, assign_role(To, Movers)};
assign_role({indirect, {Op, From}, To}, Movers) -> {indirect, {Op, assign_role(From, Movers)}, assign_role(To, Movers)}.




% to assign the sequence number of terminal condition, and the cast initiator.
assign_cond({Last, Phase, Outcome}, Mover, CurrSeq) -> {CurrSeq + Last, Mover, Phase, Outcome}.




% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description.
update_cast({Name, Cond, Spec}, {CurrSeq, _, _, {Mover, _, _}, _}, I1, I2) ->
    {Name, assign_cond(Cond, Mover, CurrSeq), assign_role(Spec, {Mover, I1, I2})}.




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


