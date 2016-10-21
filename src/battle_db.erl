-module(battle_db).

-author('Yue Marvin Tao').

-export([init_table/0, create_cast_table/0, list_all_available_casts/0]).

init_table() ->
    create_cast_table().

list_all_available_casts() ->
    lists:flatten(ets:match(casts, {'$1', '_'})).

create_cast_table() ->

    CastsGeneral = [

        {combat_sense, 1, [
            {combat_sense, {0, 1, casting}, {direct, {times, 0.2}, {role, to_attr, of_self, hit}}, none}
        ]},

        {attack_command, 1, [
            {attack_command, {0, 1, casting}, {direct, {linear, {23, 109}, 1}, {role, to_hp, of_opponent, null}}, none}
        ]},

        {burning_powder, 1, [
            {burning_powder, {0, 1, casting}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, none},
            {burning_powder, {1, 3, settling}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, none},
            {burning_powder, {4, 1, settling}, {direct, {times, 0.05}, {role, to_hp, of_opponent, null}}, none}
        ]},

        {charm_of_foresight, 1, [
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, dodge}}, none},
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, block}}, none}
        ]},

        {holy_hand_grenade, 1, [
            {holy_hand_grenade, {0, 1, casting}, {direct, {add, {1, 500}}, {role, to_hp, of_opponent, resist}}, none}
        ]},

        {talisman_of_death, 1, [
            {talisman_of_death, {0, 1, casting}, {direct, {times, 0.15}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {talisman_of_spellshrouding, 1, [
            {talisman_of_spellshrouding, {0, 1, casting}, {direct, {add, 100}, {role, to_attr, of_self, resist}}, none}
        ]},

        {poison_gas, 1, [
            {poison_gas, {0, 1, casting}, {direct, {set, stunned}, {role, to_attr, of_self, status}}, none}
        ]}
    ],

    Warrior = [
        {shield_wall, 1, [
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, dodge}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, to_attr, of_self, block}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, hit}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, critical}}, none}
        ]},

        {deadly_strike, 1, [
            {deadly_strike, {0, 1, casting}, {direct, {add, 150}, {role, to_hp, of_opponent, null}}, absorbable},
            {deadly_strike, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {double_swing, 1, [
            {double_swing, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {chain_lock, 1, [
            {chain_lock, {0, 1, casting}, {direct, {set, locked}, {role, to_attr, of_opponent, status}}, resistable}
        ]},

        {first_aid, 1, [
            {first_aid, {0, 1, casting}, {direct, {times, -0.08}, {role, to_hp, of_self, null}}, none}
        ]}
    ],


    Hunter = [
        {tornado, 1, [
            {tornado, {0, 1, casting}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, none},          
            {tornado, {1, 4, settling}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, none},          
            {tornado, {0, 1, casting}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, absorbable},          
            {tornado, {1, 4, settling}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, absorbable}
        ]},

        {mend, 1, [
            {mend, {0, 1, casting}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, none},
            {mend, {1, 2, settling}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, none}
        ]},

        {outbreak, 1, [
            {outbreak, {0, 3, attacking}, {direct, {add, 70}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {roots, 1, [
            {roots, {0, 1, casting}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable},
            {roots, {1, 2, settling}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable}
        ]},

        {tree_hide, 1, [
            {tree_hide, {0, 3, casting}, {direct, {add, 0.5}, {role, to_attr, of_self, armor}}, resistable}
        ]}
    ],

    Rogue = [
        {healing_potion, 1, [
            {healing_potion, {0, 1, casting}, {direct, {add, {-255, -175}}, {role, to_hp, of_self, null}}, resistable}
        ]},

        {pierce_armor, 1, [
            {pierce_armor, {0, 1, casting}, {direct, {add, -50}, {role, to_attr, of_opponent, armor}}, resistable},
            {pierce_armor, {1, 2, settling}, {direct, {add, -50}, {role, to_attr, of_opponent, armor}}, resistable}
        ]},

        {flurry, 1, [
            {flurry, {0, 1, casting}, {direct, {set, 3}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {spellbreak, 1, [
            {spellbreak, {0, 1, casting}, {direct, {times, 0.7}, {role, to_attr, of_self, resist}}, none},
            {spellbreak, {1, 3, settling}, {direct, {times, 0.7}, {role, to_attr, of_self, resist}}, none}
        ]},

        {perfect_strike, [
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, dodge}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, block}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, hit}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, to_attr, of_self, critical}}, none}
        ]}
    ],

    Mage = [
        {vampiric_bolt, 1, [
            {vampiric_bolt, {0, 1, casting}, {indirect, {linear, {role, from_hp, of_opponent, null}, -0.1}, {role, to_hp, of_self, null}}, resistable},
            {vampiric_bolt, {0, 1, casting}, {direct, {times, 0.1}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {arcane_surge, 1, [
            {arcane_surge, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {lower_resist, 1, [
            {lower_resist, {0, 1, casting}, {set, stunned}, {role, to_attr, of_opponent, status}, none},
            {lower_resist, {0, 1, casting}, {direct, {times, -0.3}, {role, to_attr, of_opponent, resist}}, none},
            {lower_resist, {1, 2, settling}, {direct, {times, -0.3}, {role, to_attr, of_opponent, resist}}, none}
        ]},

        {pyromania, 1, [
            {pyromania, {0, 1, casting}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, resistable},
            {pyromania, {1, 2, settling}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, resistable},
            {pyromania, {0, 1, casting}, {direct, {times, -0.5}, {role, to_attr, of_opponent, critical}}, resistable}
        ]},

        {mind_blast, 1, [
            {mind_blast, {0, 1, casting}, {direct, {add, 125}, {role, to_hp, of_opponent, null}}, resistable},
            {mind_blast, {0, 1, casting}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable}
        ]}
    ],

    ets:new(casts, [set, named_table]),
    ets:insert(casts, CastsGeneral).


