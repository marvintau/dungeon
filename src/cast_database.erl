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
            {1, off, [
                {
                 {{0, null, attacking}, [{block, '==', {ref, outcome, off}}]},
                 [{{add, {-125}, absorbable},{ref, hp, def}}]
                }
            ]}
        ]},

        {blade_dance, talent, [
            {1, off, [
                {
                 {{0, null, settling}, []},
                 [{{add_mul, {0.1}, none}, {ref, critical, off}}, {{add_mul, {0.5}, none}, {ref, critical_multiplier, off}}]
                }
            ]}
        ]},

        {freeze, talent, [
            {1, def, [
                {
                 {{0, 2, settling}, []},
                 [{{set, {1}, none}, {ref, cast_disabled, def}},
                  {{set, {1}, none}, {ref, attack_disabled, def}},
                  {{set, {0}, none}, {ref, dodge, def}},
                  {{set, {0}, none}, {ref, block, def}},
                  {{set, {0}, none}, {ref, resist, def}},
                  {{set, {119}, none}, {ref, critical, off}}
                 ]
                }
            ]}
        ]},

        {assault, talent, [
            {1, off, [
                {
                 {{0, null, attacking}, [{dodge, '==', {ref, outcome, def}}]},
                 [{{add, {1}, none}, {ref, rem_moves, off}},
                  {{set, {0}, none}, {ref, dodge, def}},
                  {{set, {0}, none}, {ref, block, def}},
                  {{set, {0}, none}, {ref, resist, def}}
                 ]
                },
                {
                 {{0, null, attacking}, [{block, '==', {ref, outcome, def}}]},
                 [{{add, {1}, none}, {ref, rem_moves, off}},
                  {{set, {0}, none}, {ref, dodge, def}},
                  {{set, {0}, none}, {ref, block, def}},
                  {{set, {0}, none}, {ref, resist, def}}
                 ]
                }
             ]}
        ]}
    ],

    CastsGeneral = [

        {rune_of_the_void, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, none}, {ref, cast_disabled, def}} ]}
            ]}
        ]},

        {holy_hand_grenade, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add, {{-500, -1}}, none}, {ref, hp, def}}]}
            ]}
        ]},

        {talisman_of_death, general, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add_mul, {-0.15}, resistable}, {ref, hp, def}}]}
            ]}
        ]},

        {talisman_of_spellshrouding, general, [
            {1, off, [
                { {{0, 2, casting}, []}, [{{add, {30}, none}, {ref, resist, off}}]}
            ]}
        ]},

        {poison_gas, general, [
            {0.5, def, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {1}, none}, {ref, attack_disabled, def}},
                  {{set, {1}, none}, {ref, cast_disabled, def}},
                  {{set, {0}, none}, {ref, dodge, def}},
                  {{set, {0}, none}, {ref, block, def}}
                 ]}
            ]},
            {0.5, off, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {1}, none}, {ref, attack_disabled, off}},
                  {{set, {1}, none}, {ref, cast_disabled, off}},
                  {{set, {0}, none}, {ref, dodge, off}},
                  {{set, {0}, none}, {ref, block, off}}
                 ]}
            ]}
        ]}
    ],

    Warrior = [
        {shield_wall, warrior, [
            {1, off, [
                { {{0, 1, casting}, []},
                [{{set, {0}, none}, {ref, dodge, off}},
                 {{set, {119}, none}, {ref, block, off}},
                 {{set, {0}, none}, {ref, hit_bonus, def}},
                 {{set, {0}, none}, {ref, critical, def}}
                ]}
            ]}
        ]},

        {sure_hit, warrior, [
            {2, off, [
                {
                 {{0, 2, casting}, []},
                 [{{set, {0}, none}, {ref, resist, def}},
                  {{set, {0}, none}, {ref, block, def}},
                  {{set, {0}, none}, {ref, dodge, def}},
                  {{set, {0}, none}, {ref, critical, off}}]}
           ]}
        ]},

        {double_swing, warrior, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {2}, none}, {ref, rem_moves, off}}]}
            ]}
        ]},

        {chain_lock, warrior, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {ref, attack_disabled, def}}]}
            ]}
        ]},

        {first_aid, warrior, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add_mul, {0.08}, none}, {ref, hp, off}}]}
            ]}
        ]}
    ],


    Hunter = [
        {tornado, hunter, [
            {1, def, [
                { {{0, 5, casting}, []}, [{{add_mul, {-0.05}, none}, {ref, hit_bonus, def}}]},
                { {{0, 5, casting}, []}, [{{add, {-50}, absorbable}, {ref, hp, def}}]}
            ]}
        ]},

        {mend, hunter, [
            {1, off, [
                { {{0, 3, casting}, []}, [{{add_mul, {0.07}, none}, {ref, hp, off}}]}
            ]}
        ]},

        {outbreak, hunter, [
            {1, def, [
                { {{0, 3, attacking}, []}, [{{add, {-70}, resistable}, {ref, hp, def}}]}
            ]}
        ]},

        {roots, hunter, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {ref, rem_moves, def}}]}
            ]}
        ]},

        {tree_hide, hunter, [
            {1, off, [
                { {{0, 3, casting}, []}, [{{add_mul, {0.7}, resistable}, {ref, armor, off}}]}
            ]}
        ]}
    ],

    Rogue = [
        {healing_potion, rogue, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {{175, 255}}, none}, {ref, hp, off}}]}
            ]}
        ]},

        {pierce_armor, rogue, [
            {1, def, [
                { {{0, 2, casting}, []}, [{{add_mul, {-0.5}, resistable}, {ref, armor, def}}]}
            ]}
        ]},

        {flurry, rogue, [
            {1, off, [
                { {{0, 2, casting}, []}, [{{set, {3}, none}, {ref, rem_moves, off}}]}
            ]}
        ]},

        {spellbreak, rogue, [
            {1, off, [
                { {{0, 2, casting}, []}, [{{add, {70}, none}, {ref, resist, off}}]}
            ]}
        ]},

        {perfect_strike, rogue, [
            {1, off, [
                { {{0, 1, casting}, []}, [
                {{set, {0}, none}, {ref, dodge, def}},
                {{set, {0}, none}, {ref, block, def}},
                {{set, {0}, none}, {ref, hit_bonus, off}},
                {{set, {119}, none}, {ref, critical, off}}]}
            ]}
        ]}
    ],

    Mage = [
        {vampiric_bolt, mage, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add_inc_mul, {{ref, hp, def}, 0.1}, none}, {ref, hp, off}}, {{add_mul, {-0.1}, none}, {ref, hp, def}}]}
            ]}
        ]},

        {arcane_surge, mage, [
            {1, off, [
                { {{0, 1, casting}, []}, [{{add, {2}, none}, {ref, rem_moves, off}}]}
            ]}
        ]},

        {lower_resist, mage, [
            {1, off, [
                { {{0, 1, casting}, []}, 
                  [{{set, {1}, none}, {ref, attack_disabled, off}},
                   {{set, {1}, none}, {ref, cast_disabled, off}},
                   {{set, {0}, none}, {ref, dodge, off}},
                   {{set, {0}, none}, {ref, block, off}}
                  ]}
            ]},
            {1, def, [
                { {{0, 3, casting}, []}, [
                    {{add_mul, {0.3}, none}, {ref, resist, def}}
                ]}
            ]}
        ]},

        {pyromania, mage, [
            {1, def, [
                { {{0, 3, casting}, []}, [{{add, {-50}, resistable}, {ref, hp, def}}]},
                { {{0, 1, casting}, []}, [{{add_mul, {-0.5}, resistable}, {ref, critical, def}}]}
            ]}
        ]},

        {mind_blast, mage, [
            {1, def, [
                { {{0, 1, casting}, []}, [{{add, {-125}, resistable}, {ref, hp, def}}]},
                { {{0, 1, casting}, []}, [{{set, {1}, resistable}, {ref, rem_moves, def}}]}
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

