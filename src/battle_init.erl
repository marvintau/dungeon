-module(battle_init).

-author('Yue Marvin Tao').

-export([create_cast_table/0]).

create_cast_table() ->

    AvailableCasts = [

        %{combat_sense, [
        %    {combat_sense, {1, casting, nah}, {direct, {times, 1.2}, {role, to_attr, of_self, hit}}, 1, none}
        %]},

        {talisman_of_death, [
            {talisman_of_death_effect, {1, casting, nah}, {direct, {times, 0.85}, {role, to_hp, of_opponent, null}}, 1, resistable}
        ]},

        {talisman_of_spellshrouding, [
            {talisman_of_spellshrouding_effect, {1, casting, nah}, {direct, {add, 100}, {role, to_attr, of_self, resist}}, 1, none}
        ]},

        

        {assault, [
                   {assault_effect, {1, casting, nah}, {direct, {add, -1}, {role, to_hp, of_opponent, null}}, 1, absorbable},
                   {assault_effect, {2, settling, nah}, {direct, {add, -1}, {role, to_hp, of_opponent, null}}, 1, absorbable}
                  ]},
        {heal, [{heal_effect, {1, casting, nah}, {direct, {add, 1}, {role, to_hp, of_self, null}}, 1, absorbable}]},
        {steal, [{steal_effect, {1, casting, nah}, {indirect, {add, {role, from_hp, of_opponent, null}}, {role, to_hp, of_self, null}}, 1, none}]}
    ],

    ets:new(casts, [set, named_table]),
    ets:insert(casts, AvailableCasts).



