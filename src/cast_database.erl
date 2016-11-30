-module(cast_database).

-author('Yue Marvin Tao').

-export([init_table/0, create_casts/0]).

-export([update_casts/1]).

init_table() ->
    ets:new(casts, [set, public, named_table]),
    create_casts().

update_casts(Data) ->
    Res = casts_to_erlang:casts(Data),
    error_logger:info_report(Res),
    true = ets:delete_all_objects(casts),
    ets:insert(casts, Res),
    ok.

create_casts() ->
    true = ets:delete_all_objects(casts),

    Talents = [
        {brave_shield_counterback, talent, [
            {1, def, [
                {
                 {{0, null, attacking}, [{block, '==', {attr, outcome, off}}]},
                 [{{add, {-125}, absorbable},{attr, hp, def}}]
                }
            ]}
        ]},

        {blade_dance, talent, [
            {1, off, [
                {
                 {{0, null, settling}, []},
                 [{{add_mul, {0.1}, none}, {attr, critical, off}}, {{add_mul, {0.5}, none}, {attr, critical_multiplier, off}}]
                }
            ]}
        ]},

        {freeze, talent, [
            {1, def, [
                {
                 {{0, 2, settling}, []},
                 [{{set, {1}, none}, {attr, cast_disabled, def}},
                  {{set, {1}, none}, {attr, attack_disabled, def}},
                  {{set, {0}, none}, {attr, dodge, def}},
                  {{set, {0}, none}, {attr, block, def}},
                  {{set, {0}, none}, {attr, resist, def}},
                  {{set, {119}, none}, {attr, critical, off}}
                 ]
                }
            ]}
        ]},

        {assault, talent, [
            {1, def, [
                {
                 {{0, null, attacking}, [{dodge, '==', {attr, outcome, def}}]},
                 [{{add, {1}, none}, {attr, rem_moves, off}},
                  {{set, {0}, none}, {attr, dodge, def}},
                  {{set, {0}, none}, {attr, block, def}},
                  {{set, {0}, none}, {attr, resist, def}}
                 ]
                },
                {
                 {{0, null, attacking}, [{block, '==', {attr, outcome, def}}]},
                 [{{add, {1}, none}, {attr, rem_moves, off}},
                  {{set, {0}, none}, {attr, dodge, def}},
                  {{set, {0}, none}, {attr, block, def}},
                  {{set, {0}, none}, {attr, resist, def}}
                 ]
                }
             ]}
        ]}
    ],

    CastsGeneral = [

        {rune_of_the_void, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, none}, {attr, cast_disabled, def}} ]}
            ]}
        ]},

        {holy_hand_grenade, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add, {{-500, -1}}, none}, {attr, hp, def}}]}
            ]}
        ]},

        {talisman_of_death, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add_mul, {-0.15}, resistable}, {attr, hp, def}}]}
            ]}
        ]},

        {talisman_of_spellshrouding, general, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add_mul, {1}, none}, {attr, resist, off}}]}
            ]}
        ]},

        {poison_gas, general, [
            {0.5, def, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {1}, none}, {attr, attack_disabled, def}},
                  {{set, {1}, none}, {attr, cast_disabled, def}},
                  {{set, {0}, none}, {attr, dodge, def}},
                  {{set, {0}, none}, {attr, block, def}}
                 ]}
            ]},
            {0.5, off, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {1}, none}, {attr, attack_disabled, off}},
                  {{set, {1}, none}, {attr, cast_disabled, off}},
                  {{set, {0}, none}, {attr, dodge, off}},
                  {{set, {0}, none}, {attr, block, off}}
                 ]}
            ]}
        ]}
    ],

    Warrior = [
        {shield_wall, warrior, [
            {1, off, [
                { {{0, 1, casting}, []},
                [{{set, {0}, none}, {attr, dodge, off}},
                 {{set, {119}, none}, {attr, block, off}},
                 {{set, {0}, none}, {attr, hit_bonus, def}},
                 {{set, {0}, none}, {attr, critical, def}}
                ]}
            ]}
        ]},

        {sure_hit, warrior, [
            {1, off, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {0}, none}, {attr, resist, def}},
                  {{set, {0}, none}, {attr, block, def}},
                  {{set, {0}, none}, {attr, dodge, def}},
                  {{set, {0}, none}, {attr, critical, off}}]}
           ]}
        ]},

        {double_swing, warrior, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {2}, none}, {attr, rem_moves, off}}]}
            ]}
        ]},

        {chain_lock, warrior, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {attr, attack_disabled, def}}]}
            ]}
        ]},

        {first_aid, warrior, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add_mul, {0.08}, none}, {attr, hp, off}}]}
            ]}
        ]}
    ],


    Hunter = [
        {tornado, hunter, [
            {1, def, [
                { {{0, 5, casting}, []}, [{{add_mul, {-0.05}, none}, {attr, hit_bonus, def}}]},
                { {{0, 5, casting}, []}, [{{add, {-50}, absorbable}, {attr, hp, def}}]}
            ]}
        ]},

        {mend, hunter, [
            {1, off, [
                { {{0, 3, casting}, []}, [{{add_mul, {0.07}, none}, {attr, hp, off}}]}
            ]}
        ]},

        {outbreak, hunter, [
            {1, def, [
                { {{0, 3, attacking}, []}, [{{add, {-70}, resistable}, {attr, hp, def}}]}
            ]}
        ]},

        {roots, hunter, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {attr, rem_moves, def}}]}
            ]}
        ]},

        {tree_hide, hunter, [
            {1, off, [
                { {{0, 3, casting}, []}, [{{add_mul, {0.7}, resistable}, {attr, armor, off}}]}
            ]}
        ]}
    ],

    Rogue = [
        {healing_potion, rogue, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {{175, 255}}, none}, {attr, hp, off}}]}
            ]}
        ]},

        {pierce_armor, rogue, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add_mul, {-0.5}, resistable}, {attr, armor, def}}]}
            ]}
        ]},

        {flurry, rogue, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{set, {3}, none}, {attr, rem_moves, off}}]}
            ]}
        ]},

        {spellbreak, rogue, [
            {1, off, [
                { {{0, 4, casting}, []}, [{{add, {70}, none}, {attr, resist, off}}]}
            ]}
        ]},

        {perfect_strike, rogue, [
            {1, off, [
                { {{0, 1, casting}, []}, [
                {{set, {0}, none}, {attr, dodge, def}},
                {{set, {0}, none}, {attr, block, def}},
                {{set, {0}, none}, {attr, hit_bonus, off}},
                {{set, {119}, none}, {attr, critical, off}}]}
            ]}
        ]}
    ],

    Mage = [
        {vampiric_bolt, mage, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add_inc_mul, {{attr, hp, def}, 0.1}, resistable}, {attr, hp, off}}]},
                { {{0, 1, casting}, []}, [{{add_mul, {-0.1}, resistable}, {attr, hp, def}}]}
            ]}
        ]},

        {arcane_surge, mage, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {2}, none}, {attr, rem_moves, off}}]}
            ]}
        ]},

        {lower_resist, mage, [
            {1, off, [
                { {{0, 1, casting}, []}, 
                  [{{set, {1}, none}, {attr, attack_disabled, off}},
                   {{set, {1}, none}, {attr, cast_disabled, off}},
                   {{set, {0}, none}, {attr, dodge, off}},
                   {{set, {0}, none}, {attr, block, off}}
                  ]}
            ]},
            {1, def, [
                { {{0, 3, casting}, []}, [
                    {{add_mul, {0.3}, none}, {attr, resist, def}}
                ]}
            ]}
        ]},

        {pyromania, mage, [
            {1, def, [
                { {{0, 3, casting}, []}, [{{add, {-50}, resistable}, {attr, hp, def}}]},
                { {{0, 1, casting}, []}, [{{add_mul, {-0.5}, resistable}, {attr, critical, def}}]}
            ]}
        ]},

        {mind_blast, mage, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add, {-125}, resistable}, {attr, hp, def}}]},
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {attr, rem_moves, def}}]}
            ]}
        ]}
    ],

    ets:insert(casts, Talents),
    ets:insert(casts, CastsGeneral),
    ets:insert(casts, Mage),
    ets:insert(casts, Rogue),
    ets:insert(casts, Hunter),
    ets:insert(casts, Warrior),
    ok.

