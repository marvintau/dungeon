-module(battle_db).

-author('Yue Marvin Tao').

-export([init_table/0, create_cast_table/0, list_all_available_casts/0]).

init_table() ->
    create_cast_table().

list_all_available_casts() ->
    lists:flatten(ets:match(casts, {'$1', '_'})).

create_cast_table() ->

    CastsGeneral = [

        {combat_sense, [
            {combat_sense, {0, 1, casting}, {direct, {times, 0.2}, {role, to_attr, of_self, hit}}, 1, none}
        ]},

        {attack_command, [
            {attack_command, {0, 1, casting}, {direct, {linear, {23, 109}, 1}, {role, to_hp, of_opponent, null}}, 1, none}
        ]},

        {burning_powder, [
            {burning_powder, {0, 1, casting}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, 1, none},
            {burning_powder, {1, 3, settling}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, 1, none},
            {burning_powder, {4, 1, settling}, {direct, {times, 0.05}, {role, to_hp, of_opponent, null}}, 1, none}
        ]},

        {charm_of_foresight, [
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, dodge}}, 1, none},
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, block}}, 1, none}
        ]},

        {holy_hand_grenade, [
            {holy_hand_grenade, {0, 1, casting}, {direct, {add, {1, 500}}, {role, to_hp, of_opponent, resist}}, 1, none}
        ]},

        {talisman_of_death, [
            {talisman_of_death, {0, 1, casting}, {direct, {times, 0.15}, {role, to_hp, of_opponent, null}}, 1, resistable}
        ]},

        {talisman_of_spellshrouding, [
            {talisman_of_spellshrouding, {0, 1, casting}, {direct, {add, 100}, {role, to_attr, of_self, resist}}, 1, none}
        ]},

        {poison_gas, [
            {poison_gas, {0, 1, casting}, {direct, {set, stunned}, {role, to_attr, of_self, status}}, 0.5, none}
        ]}
    ],

    Warrior = [
        {shield_wall, [
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, dodge}}, 1, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, to_attr, of_self, block}}, 1, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, hit}}, 1, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, critical}}, 1, none}
        ]},

        {deadly_strike, [
            {deadly_strike, {0, 1, casting}, {direct, {add, 150}, {role, to_hp, of_opponent, null}}, 1, absorbable},
            {deadly_strike, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, 1, none}
        ]},

        {double_swing, [
            {double_swing, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, 1, none}
        ]},

        {chain_lock, [
            {chain_lock, {0, 1, casting}, {direct, {set, locked}, {role, to_attr, of_opponent, status}}, 1, resistable}
        ]},

        {first_aid, [
            {first_aid, {0, 1, casting}, {direct, {times, -0.08}, {role, to_hp, of_self, null}}, 1, none}
        ]}
    ],


    Hunter = [
        {tornado, [
            {tornado, {0, 1, casting}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, 1, none},          
            {tornado, {1, 4, settling}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, 1, none},          
            {tornado, {0, 1, casting}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, 1, absorbable},          
            {tornado, {1, 4, settling}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, 1, absorbable}
        ]},

        {mend, [
            {mend, {0, 1, casting}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, 1, none},
            {mend, {1, 2, settling}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, 1, none}
        ]},

        {outbreak, [
            {outbreak, {0, 1, casting}}
        ]}
    ],

    ets:new(casts, [set, named_table]),
    ets:insert(casts, CastsGeneral).


