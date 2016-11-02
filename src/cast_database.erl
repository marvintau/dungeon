-module(cast_database).

-author('Yue Marvin Tao').

-export([init_table/0, create_casts/0]).

-export([update_cast/1]).

-export([remove_cast/1]).

init_table() ->
    create_casts().

update_cast(Data) ->
    Decoded = jiffy:decode(Data),
    {Name, _, _} = Res = casts_to_erlang:cast(Decoded),
    error_logger:info_report(ets:lookup(casts, Name)),
    ets:insert(casts, Res),
    error_logger:info_report(ets:lookup(casts, Name)),
    ok.

remove_cast(Data) ->
    Decoded = jiffy:decode(Data),
    {Name, _, _} = casts_to_erlang:cast(Decoded),
    error_logger:info_report(ets:lookup(casts, Name)),
    ets:delete(casts, Name),
    error_logger:info_report(length(ets:lookup(casts, Name))),
    ok.


create_casts() ->

    CastsGeneral = [

        {rune_of_the_void, general, [
            {1, [
                {rune_of_the_void, {0, 1, casting}, {direct, {set, true}, {role, attr, of_opponent, cast_disabled}}, none}
            ]}
        ]},

        {holy_hand_grenade, general, [
            {1, [
                {holy_hand_grenade, {0, 1, casting}, {direct, {add, {-500, -1}}, {role, hp, of_opponent, resist}}, none}
            ]}
        ]},

        {talisman_of_death, general, [
            {1, [
                {talisman_of_death, {0, 1, casting}, {direct, {times, -0.15}, {role, hp, of_opponent, none}}, resistable}
            ]}
        ]},

        {talisman_of_spellshrouding, general, [
            {1, [
                {talisman_of_spellshrouding, {0, 1, casting}, {direct, {add, -100}, {role, attr, of_self, resist}}, none}
            ]}
        ]},

        {poison_gas, general, [
            {0.5, [
                {poison_gas, {0, 1, casting}, {direct, {set, true}, {role, attr, of_opponent, attack_disabled}}, none},
                {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, dodge}}, none},
                {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, block}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, true}, {role, attr, of_opponent, attack_disabled}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, true}, {role, attr, of_opponent, cast_disabled}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_opponent, dodge}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_opponent, block}}, none}
            ]},
            {0.5, [
                {poison_gas, {0, 1, casting}, {direct, {set, true}, {role, attr, of_self, attack_disabled}}, none},
                {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, dodge}}, none},
                {poison_gas, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, block}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, true}, {role, attr, of_self, attack_disabled}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, true}, {role, attr, of_self, cast_disabled}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_self, dodge}}, none},
                {poison_gas, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_self, block}}, none}
            ]}
        ]}
    ],

    Warrior = [
        {shield_wall, warrior, [
            {1, [
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, dodge}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, attr, of_self, block}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, hit_bonus}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, critical}}, none}
            ]}
        ]},

        {sure_hit, warrior, [
            {1, [
                {sure_hit, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, resist}}, none},
                {sure_hit, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, block}}, none},
                {sure_hit, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, dodge}}, none},
                {sure_hit, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, critical}}, none},

                {sure_hit, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_opponent, resist}}, none},
                {sure_hit, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_opponent, block}}, none},
                {sure_hit, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_opponent, dodge}}, none},
                {sure_hit, {1, 1, settling}, {direct, {set, 0}, {role, attr, of_self, critical}}, none}

           ]}
        ]},

        {double_swing, warrior, [
            {1, [
                {double_swing, {0, 1, casting}, {direct, {set, 4}, {role, rem_moves, of_self, none}}, none}
            ]}
        ]},

        {chain_lock, warrior, [
            {1, [
                {chain_lock, {0, 1, casting}, {direct, {set, true}, {role, attr, of_opponent, attack_disabled}}, resistable}
            ]}
        ]},

        {first_aid, warrior, [
            {1, [
                {first_aid, {0, 1, casting}, {direct, {times, 0.08}, {role, hp, of_self, none}}, none}
            ]}
        ]}
    ],


    Hunter = [
        {tornado, hunter, [
            {1, [
                {tornado, {0, 1, casting}, {direct, {times, -0.05}, {role, attr, of_opponent, hit_bonus}}, none},          
                {tornado, {1, 4, settling}, {direct, {times, -0.05}, {role, attr, of_opponent, hit_bonus}}, none},          
                {tornado, {0, 1, casting}, {direct, {add, -50}, {role, hp, of_opponent, none}}, absorbable},          
                {tornado, {1, 4, settling}, {direct, {add, -50}, {role, hp, of_opponent, none}}, absorbable}
            ]}
        ]},

        {mend, hunter, [
            {1, [
                {mend, {0, 1, casting}, {direct, {times, -0.07}, {role, hp, of_self, none}}, none},
                {mend, {1, 2, settling}, {direct, {times, -0.07}, {role, hp, of_self, none}}, none}
            ]}
        ]},

        {outbreak, hunter, [
            {1, [
                {outbreak, {0, 3, attacking}, {direct, {add, -70}, {role, hp, of_opponent, none}}, resistable}
            ]}
        ]},

        {roots, hunter, [
            {1, [
                {roots, {0, 1, casting}, {direct, {set, 1}, {role, rem_moves, of_opponent, none}}, resistable},
                {roots, {1, 2, settling}, {direct, {set, 1}, {role, rem_moves, of_opponent, none}}, resistable}
            ]}
        ]},

        {tree_hide, hunter, [
            {1, [
                {tree_hide, {0, 3, casting}, {direct, {add, 0.5}, {role, attr, of_self, armor}}, resistable}
            ]}
        ]}
    ],

    Rogue = [
        {healing_potion, rogue, [
            {1, [
                {healing_potion, {0, 1, casting}, {direct, {add, {175, 255}}, {role, hp, of_self, none}}, resistable}
            ]}
        ]},

        {pierce_armor, rogue, [
            {1, [
                {pierce_armor, {0, 1, casting}, {direct, {add, -50}, {role, attr, of_opponent, armor}}, resistable},
                {pierce_armor, {1, 2, settling}, {direct, {add, -50}, {role, attr, of_opponent, armor}}, resistable}
            ]}
        ]},

        {flurry, rogue, [
            {1, [
                {flurry, {0, 1, casting}, {direct, {set, 3}, {role, rem_moves, of_self, none}}, none}
            ]}
        ]},

        {spellbreak, rogue, [
            {1, [
                {spellbreak, {0, 1, casting}, {direct, {times, 0.7}, {role, attr, of_self, resist}}, none},
                {spellbreak, {1, 3, settling}, {direct, {times, 0.7}, {role, attr, of_self, resist}}, none}
            ]}
        ]},

        {perfect_strike, rogue, [
            {1, [
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, dodge}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, block}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, hit_bonus}}, none},
                {shield_wall, {0, 1, casting}, {direct, {set, 100}, {role, attr, of_self, critical}}, none}
            ]}
        ]}
    ],

    Mage = [
        {vampiric_bolt, mage, [
            {1, [
                {vampiric_bolt, {0, 1, casting}, {indirect, {linear, {role, hp, of_opponent, none}, -0.1}, {role, hp, of_self, none}}, resistable},
                {vampiric_bolt, {0, 1, casting}, {direct, {times, 0.1}, {role, hp, of_opponent, none}}, resistable}
            ]}
        ]},

        {arcane_surge, mage, [
            {1, [
                {arcane_surge, {0, 1, casting}, {direct, {set, 4}, {role, rem_moves, of_self, none}}, none}
            ]}
        ]},

        {lower_resist, mage, [
            {1, [
                {lower_resist, {0, 1, casting}, {direct, {set, true}, {role, attr, of_self, attack_disabled}}, none},
                {lower_resist, {0, 1, casting}, {direct, {set, true}, {role, attr, of_self, cast_disabled}}, none},
                {lower_resist, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, dodge}}, none},
                {lower_resist, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, block}}, none},

                {lower_resist, {0, 1, casting}, {direct, {times, -0.3}, {role, attr, of_opponent, resist}}, none},
                {lower_resist, {1, 2, settling}, {direct, {times, -0.3}, {role, attr, of_opponent, resist}}, none}
            ]}
        ]},

        {pyromania, mage, [
            {1, [
                {pyromania, {0, 1, casting}, {direct, {add, -50}, {role, hp, of_opponent, none}}, resistable},
                {pyromania, {1, 2, settling}, {direct, {add, -50}, {role, hp, of_opponent, none}}, resistable},
                {pyromania, {0, 1, casting}, {direct, {times, -0.5}, {role, attr, of_opponent, critical}}, resistable}
            ]}
        ]},

        {mind_blast, mage, [
            {1, [
                {mind_blast, {0, 1, casting}, {direct, {add, -125}, {role, hp, of_opponent, none}}, resistable},
                {mind_blast, {0, 1, casting}, {direct, {set, 1}, {role, rem_moves, of_opponent, none}}, resistable}
            ]}
        ]}
    ],

    ets:new(casts, [set, public, named_table]),

    ets:insert(casts, CastsGeneral),
    ets:insert(casts, Warrior),
    ets:insert(casts, Rogue),
    ets:insert(casts, Mage),
    ets:insert(casts, Hunter).


