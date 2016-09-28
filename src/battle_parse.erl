-module(battle_parse).

-author('Yue Marvin Tao').

-export([player_context_from_parsed_JSON/1]).

weapon_attr(Weapon) ->
    case binary_to_atom(Weapon, utf8) of
        physical -> {damage, physical};
        mage -> {damage, mage};
        shield -> {no_damage, shield};
        bare -> {no_damage, bare}
    end.

parse_single_player(SinglePlayerData) ->

    {[
      {_, ID}, {_, HP}, {_, PrimType}, {_, PrimMax}, {_, PrimMin}, {_, SecdType},
      {_, SecdMax}, {_, SecdMin}, {_, Armor}, {_, Hit}, {_, Critic}, {_, Dodge},
      {_, Resist}, {_, Block}, {_, Agi}, {_, CastList}
     ]} = SinglePlayerData,

    #{

            id         => binary_to_atom(ID, utf8),
            hp         => HP,
            rem_attacks => 2,

            prim_hand  => {prim, weapon_attr(PrimType), {PrimMin, PrimMax}},
            secd_hand  => {secd, weapon_attr(SecdType), {SecdMin, SecdMax}},
            curr_hand  => {prim, weapon_attr(PrimType), {PrimMin, PrimMax}},

            cast_list => lists:map(fun(X) -> binary_to_atom(X, utf8) end, CastList),

            curr_attr => #{
                armor      => Armor,
                hit        => Hit,
                critic     => Critic,
                dodge      => Dodge,
                resist     => Resist,
                block      => Block,
                agility    => Agi,
                outcome    => null,
                damage_dealt => 0

            },
           
            orig_attr => #{
                armor      => Armor,
                hit        => Hit,
                critic     => Critic,
                dodge      => Dodge,
                resist     => Resist,
                block      => Block,
                agility    => Agi,
                outcome    => null,
                damage_dealt => 0
             },

            damage_coeff => 1
        }.



player_context_from_parsed_JSON(Data) ->

    {[{<<"player1">>, Player1}, {<<"player2">>, Player2}]} = Data,

    {parse_single_player(Player1), parse_single_player(Player2)}.
