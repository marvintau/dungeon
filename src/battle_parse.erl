-module(battle_parse).

-author('Yue Marvin Tao').

-export([player_context_from_parsed_JSON/1]).

parse_single_player(SinglePlayerData) ->

    {[
      {_, ID}, {_, HP}, {_, PrimType}, {_, PrimMax}, {_, PrimMin}, {_, SecdType},
      {_, SecdMax}, {_, SecdMin}, {_, Armor}, {_, Hit}, {_, Critic}, {_, Dodge},
      {_, Resist}, {_, Block}, {_, Agi}, {_, CastList}
     ]} = SinglePlayerData,

    #{

            id         => binary_to_atom(ID, utf8),
            hp         => HP,
            rem_moves  => 2,

            prim_hand  => {prim, binary_to_atom(PrimType, utf8), {PrimMin, PrimMax}},
            secd_hand  => {secd, binary_to_atom(SecdType, utf8), {SecdMin, SecdMax}},
            curr_hand  => {prim, binary_to_atom(PrimType, utf8), {PrimMin, PrimMax}},

            cast_list => lists:map(fun(X) -> binary_to_atom(X, utf8) end, CastList),

            curr_attr => #{
                armor      => Armor,
                hit        => Hit,
                critical   => Critic,
                dodge      => Dodge,
                resist     => Resist,
                block      => Block,
                agility    => Agi,
                outcome    => null,
                damage_taken => 0

            },
           
            orig_attr => #{
                armor      => Armor,
                hit        => Hit,
                critical   => Critic,
                dodge      => Dodge,
                resist     => Resist,
                block      => Block,
                agility    => Agi,
                outcome    => null,
                damage_taken => 0
             },

            damage_coeff => 1
        }.



player_context_from_parsed_JSON(Data) ->

    {[{<<"player1">>, Player1}, {<<"player2">>, Player2}]} = Data,

    {parse_single_player(Player1), parse_single_player(Player2)}.
