-module(battle_init).

-author('Yue Marvin Tao').

-export([create_cast_table/0]).

create_cast_table() ->

    AvailableCasts = [
        {assault, [
                   {assault_effect, {1, casting, nah}, {direct, {add, -1}, {role, to_hp, of_opponent, null}}},
                   {assault_effect, {1, settling, nah}, {direct, {add, -1}, {role, to_hp, of_opponent, null}}}
                  ]},
        {heal, [{heal_effect, {1, casting, nah}, {direct, {add, 1}, {role, to_hp, of_self, null}}}]},
        {steal, [{steal_effect, {1, casting, nah}, {indirect, {add, {role, from_hp, of_opponent, null}}, {role, to_hp, of_self, null}}}]}
    ],

    ets:new(casts, [set, named_table]),
    ets:insert(casts, AvailableCasts).



