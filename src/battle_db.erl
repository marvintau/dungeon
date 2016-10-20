-module(battle_db).

-author('Yue Marvin Tao').

-export([init_table/0, create_cast_table/0, list_all_available_casts/0]).

init_table() ->
    create_cast_table().

list_all_available_casts() ->
    lists:flatten(ets:match(casts, {'$1', '_'})).

create_cast_table() ->

    AvailableCasts = [

        {combat_sense, [
            {combat_sense, {0, 1, casting, nah}, {direct, {times, 1.2}, {role, to_attr, of_self, hit}}, 1, none}
        ]},

        {attack_command, [
            {attack_command, {0, 1, casting, nah}, {direct, {linear, {23, 109}, -1}, {role, to_hp, of_opponent, null}}, 1, none}
        ]},

        {burning_powder, [
            {burning_powder, {0, 1, casting, nah}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, 1, none},
            {burning_powder, {1, 3, settling, nah}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, 1, none},
            {burning_powder, {4, 1, settling, nah}, {direct, {times, 0.05}, {role, to_hp, of_opponent, null}}, 1, none}
        ]},

        {talisman_of_death, [
            {talisman_of_death, {0, 1, casting, nah}, {direct, {times, 0.15}, {role, to_hp, of_opponent, null}}, 1, resistable}
        ]},

        {talisman_of_spellshrouding, [
            {talisman_of_spellshrouding, {0, 1, casting, nah}, {direct, {add, 100}, {role, to_attr, of_self, resist}}, 1, none}
        ]}

        

    ],

    ets:new(casts, [set, named_table]),
    ets:insert(casts, AvailableCasts).



