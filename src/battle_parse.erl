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
      {_, Resist}, {_, Block}, {_, Agi}, {_, Cast}
     ]} = SinglePlayerData,

    #{
            id         => binary_to_atom(ID, utf8),
            hp         => HP,
            prim_type  => weapon_attr(PrimType),
            prim_range => {PrimMin, PrimMax},
            secd_type  => weapon_attr(SecdType),
            secd_range => {SecdMin, SecdMax},

            damage_coeff => 1,

            armor      => Armor,
            hit        => Hit,
            critic     => Critic,
            dodge      => Dodge,
            resist     => Resist,
            block      => Block,
            agility    => Agi,

            curr_hand   => {prim, weapon_attr(PrimType), {PrimMin, PrimMax}},

            curr_cast => binary_to_atom(Cast, utf8)
           }.



player_context_from_parsed_JSON(Data) ->

    {[{<<"player1">>, Player1}, {<<"player2">>, Player2}]} = Data,

    {parse_single_player(Player1), parse_single_player(Player2)}.
