-module(battle_db).

-author('Yue Marvin Tao').

-export([init_table/0, create_casts/0, list_casts/1, list_cast_json/1]).

init_table() ->
    create_casts().

list_casts(Class) ->
    General = lists:flatten(ets:match(casts, {'$1', general, '_', '_'})),
    ClassCast = lists:flatten(ets:match(casts, {'$1', Class, '_', '_'})),
    lists:append(General, ClassCast).

list_cast_json(Data) ->
    {[{<<"id">>, _ID}, {<<"class">>, Class}]} = Data,
    ReturnedData = [ none | list_casts(binary_to_atom(Class, utf8))],    
    error_logger:info_report(ReturnedData),
    {done, jiffy:encode(ReturnedData)}.

create_casts() ->

    CastsGeneral = [

        {combat_sense, general, 1, [
            {combat_sense, {0, 1, casting}, {direct, {times, 0.2}, {role, to_attr, of_self, hit}}, none}
        ]},

        {attack_command, general, 1, [
            {attack_command, {0, 1, casting}, {direct, {linear, {23, 109}, 1}, {role, to_hp, of_opponent, null}}, none}
        ]},

        {burning_powder, general, 1, [
            {burning_powder, {0, 1, casting}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, none},
            {burning_powder, {1, 3, settling}, {direct, {times, 0.01}, {role, to_hp, of_opponent, null}}, none},
            {burning_powder, {4, 1, settling}, {direct, {times, 0.05}, {role, to_hp, of_opponent, null}}, none}
        ]},

        {charm_of_foresight, general, 1, [
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, dodge}}, none},
            {charm_of_foresight, {0, 1, casting}, {direct, {times, 0.25}, {role, to_attr, of_self, block}}, none}
        ]},

        {holy_hand_grenade, general, 1, [
            {holy_hand_grenade, {0, 1, casting}, {direct, {add, {1, 500}}, {role, to_hp, of_opponent, resist}}, none}
        ]},

        {talisman_of_death, general, 1, [
            {talisman_of_death, {0, 1, casting}, {direct, {times, 0.15}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {talisman_of_spellshrouding, general, 1, [
            {talisman_of_spellshrouding, {0, 1, casting}, {direct, {add, 100}, {role, to_attr, of_self, resist}}, none}
        ]},

        {poison_gas, general, 0.5, [
            {poison_gas, {0, 1, casting}, {direct, {set, false}, {role, to_attr, of_opponent, is_movable}}, none},
            {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, dodge}}, none},
            {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, block}}, none}
        ]}
    ],

    Warrior = [
        {shield_wall, warrior, 1, [
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, dodge}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, to_attr, of_self, block}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, hit}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, critical}}, none}
        ]},

        {deadly_strike, warrior, 1, [
            {deadly_strike, {0, 1, casting}, {direct, {add, 150}, {role, to_hp, of_opponent, null}}, absorbable},
            {deadly_strike, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {double_swing, warrior, 1, [
            {double_swing, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {chain_lock, warrior, 1, [
            {chain_lock, {0, 1, casting}, {direct, {set, locked}, {role, to_attr, of_opponent, status}}, resistable}
        ]},

        {first_aid, warrior, 1, [
            {first_aid, {0, 1, casting}, {direct, {times, -0.08}, {role, to_hp, of_self, null}}, none}
        ]}
    ],


    Hunter = [
        {tornado, hunter, 1, [
            {tornado, {0, 1, casting}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, none},          
            {tornado, {1, 4, settling}, {direct, {times, -0.05}, {role, to_attr, of_opponent, hit}}, none},          
            {tornado, {0, 1, casting}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, absorbable},          
            {tornado, {1, 4, settling}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, absorbable}
        ]},

        {mend, hunter, 1, [
            {mend, {0, 1, casting}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, none},
            {mend, {1, 2, settling}, {direct, {times, -0.07}, {role, to_hp, of_self, null}}, none}
        ]},

        {outbreak, hunter, 1, [
            {outbreak, {0, 3, attacking}, {direct, {add, 70}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {roots, hunter, 1, [
            {roots, {0, 1, casting}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable},
            {roots, {1, 2, settling}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable}
        ]},

        {tree_hide, hunter, 1, [
            {tree_hide, {0, 3, casting}, {direct, {add, 0.5}, {role, to_attr, of_self, armor}}, resistable}
        ]}
    ],

    Rogue = [
        {healing_potion, rogue, 1, [
            {healing_potion, {0, 1, casting}, {direct, {add, {-255, -175}}, {role, to_hp, of_self, null}}, resistable}
        ]},

        {pierce_armor, rogue, 1, [
            {pierce_armor, {0, 1, casting}, {direct, {add, -50}, {role, to_attr, of_opponent, armor}}, resistable},
            {pierce_armor, {1, 2, settling}, {direct, {add, -50}, {role, to_attr, of_opponent, armor}}, resistable}
        ]},

        {flurry, rogue, 1, [
            {flurry, {0, 1, casting}, {direct, {set, 3}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {spellbreak, rogue, 1, [
            {spellbreak, {0, 1, casting}, {direct, {times, 0.7}, {role, to_attr, of_self, resist}}, none},
            {spellbreak, {1, 3, settling}, {direct, {times, 0.7}, {role, to_attr, of_self, resist}}, none}
        ]},

        {perfect_strike, rogue, 1, [
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, dodge}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_opponent, block}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, hit}}, none},
            {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, to_attr, of_self, critical}}, none}
        ]}
    ],

    Mage = [
        {vampiric_bolt, mage, 1, [
            {vampiric_bolt, {0, 1, casting}, {indirect, {linear, {role, from_hp, of_opponent, null}, -0.1}, {role, to_hp, of_self, null}}, resistable},
            {vampiric_bolt, {0, 1, casting}, {direct, {times, 0.1}, {role, to_hp, of_opponent, null}}, resistable}
        ]},

        {arcane_surge, mage, 1, [
            {arcane_surge, {0, 1, casting}, {direct, {set, 4}, {role, to_rem_moves, of_self, null}}, none}
        ]},

        {lower_resist, mage, 1, [
            {lower_resist, {0, 1, casting}, {direct, {set, false}, {role, to_attr, of_self, is_movable}}, none},
            {lower_resist, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, dodge}}, none},
            {lower_resist, {0, 1, casting}, {direct, {set, 0}, {role, to_attr, of_self, block}}, none},
            {lower_resist, {0, 1, casting}, {set, stunned}, {role, to_attr, of_opponent, status}, none},
            {lower_resist, {0, 1, casting}, {direct, {times, -0.3}, {role, to_attr, of_opponent, resist}}, none},
            {lower_resist, {1, 2, settling}, {direct, {times, -0.3}, {role, to_attr, of_opponent, resist}}, none}
        ]},

        {pyromania, mage, 1, [
            {pyromania, {0, 1, casting}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, resistable},
            {pyromania, {1, 2, settling}, {direct, {add, 50}, {role, to_hp, of_opponent, null}}, resistable},
            {pyromania, {0, 1, casting}, {direct, {times, -0.5}, {role, to_attr, of_opponent, critical}}, resistable}
        ]},

        {mind_blast, mage, 1, [
            {mind_blast, {0, 1, casting}, {direct, {add, 125}, {role, to_hp, of_opponent, null}}, resistable},
            {mind_blast, {0, 1, casting}, {direct, {set, 1}, {role, to_rem_moves, of_opponent, null}}, resistable}
        ]}
    ],

    ets:new(casts, [set, named_table]),

    ets:insert(casts, CastsGeneral),
    ets:insert(casts, Warrior),
    ets:insert(casts, Rogue),
    ets:insert(casts, Mage),
    ets:insert(casts, Hunter).


