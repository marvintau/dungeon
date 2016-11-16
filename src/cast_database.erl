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

    Talents = [
        {brave_shield_counterback, talent, [
            {1, [
                {
                 {{0, null, attacking}, [{attack, '==', {attr, outcome, def}}]},
                 [{{add, -125, absorbable},{state, hp, def}}]
                }
            ]}
        ]},

        {blade_dance, talent, [
            {1, [
                {
                 {{0, null, settling}, []},
                 [{{add_mul, 0.1, none}, {attr, critical, off}}, {{add_mul, 0.5, none}, {attr, critical_multiplier, off}}]
                }
            ]}
        ]},

        {freeze, talent, [
            {1, [
                {
                 {{0, 2, settling}, []},
                 [{{set, true, none}, {attr, cast_disabled, def}},
                  {{set, true, none}, {attr, attack_disabled, off}},
                  {{set, 0, none}, {attr, dodge, def}},
                  {{set, 0, none}, {attr, block, def}},
                  {{set, 0, none}, {attr, resist, def}},
                  {{set, 120, none}, {attr, critical, off}}
                 ]
                }
            ]}
        ]},

        {assault, talent, [
            {1, [
                {
                 {{0, null, settling}, [{dodge, '==', {attr, outcome, def}}]},
                 [{{add, 1, none}, {state, rem_moves, off}},
                  {{set, 0, none}, {attr, dodge, def}},
                  {{set, 0, none}, {attr, block, def}},
                  {{set, 0, none}, {attr, resist, def}}
                 ]
                },
                {
                 {{0, null, settling}, [{block, '==', {attr, outcome, def}}]},
                 [{{add, 1, none}, {state, rem_moves, off}},
                  {{set, 0, none}, {attr, dodge, def}},
                  {{set, 0, none}, {attr, block, def}},
                  {{set, 0, none}, {attr, resist, def}}
                 ]
                }
             ]}
        ]}
    ],

    CastsGeneral = [

        {rune_of_the_void, general, [
            {1, [
                {
                {{0, 1, casting}, []},
                [{{set, true, none}, {attr, cast_disabled, def}}
                ]}
            ]}
        ]},

        {holy_hand_grenade, general, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, {-500, -1}, none}, {state, hp, def}}]}
            ]}
        ]},

        {talisman_of_death, general, [
            {1, [
                { {{0, 1, casting}, []}, [{{add_mul, -0.15, resistable}, {state, hp, def}}]}
            ]}
        ]},

        {talisman_of_spellshrouding, general, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, -100, none}, {attr, resist, off}}]}
            ]}
        ]},

        {poison_gas, general, [
            {0.5, [
                {
                 {{0, 2, casting}, []},
                 [{{set, true, none}, {attr, attack_disabled, def}},
                  {{set, true, none}, {attr, cast_disabled, def}},
                  {{set, 0, none}, {attr, dodge, def}},
                  {{set, 0, none}, {attr, block, def}}
                 ]}
            ]},
            {0.5, [
                {
                 {{0, 2, casting}, []},
                 [{{set, true, none}, {attr, attack_disabled, off}},
                  {{set, true, none}, {attr, cast_disabled, off}},
                  {{set, 0, none}, {attr, dodge, off}},
                  {{set, 0, none}, {attr, block, off}}
                 ]}
            ]}
        ]}
    ],

    Warrior = [
        {shield_wall, warrior, [
            {1, [
                { {{0, 1, casting}, []},
                [{{set, 0, none}, {attr, dodge, off}},
                 {{set, 100, none}, {attr, block, off}},
                 {{set, 0, none}, {attr, hit_bonus, def}},
                 {{set, 0, none}, {attr, critical, def}}
                ]}
            ]}
        ]},

        {sure_hit, warrior, [
            {1, [
                {
                 {{0, 2, casting}, []},
                 [{{set, 0, none}, {attr, resist, def}},
                  {{set, 0, none}, {attr, block, def}},
                  {{set, 0, none}, {attr, dodge, def}},
                  {{set, 0, none}, {attr, critical, off}}]}
           ]}
        ]},

        {double_swing, warrior, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, 2, none}, {state, rem_moves, off}}]}
            ]}
        ]},

        {chain_lock, warrior, [
            {1, [
                { {{0, 1, casting}, []}, [{{set, true, resistable}, {attr, attack_disabled, def}}]}
            ]}
        ]},

        {first_aid, warrior, [
            {1, [
                { {{0, 1, casting}, []}, [{{add_mul, 0.08, none}, {state, hp, off}}]}
            ]}
        ]}
    ],


    Hunter = [
        {tornado, hunter, [
            {1, [
                { {{0, 5, casting}, []}, [{{add_mul, -0.05, none}, {attr, hit_bonus, def}}]}
            ]}
        ]},

        {mend, hunter, [
            {1, [
                { {{0, 3, casting}, []}, [{{add_mul, 0.07, none}, {state, hp, off}}]}
            ]}
        ]},

        {outbreak, hunter, [
            {1, [
                { {{0, 3, attacking}, []}, [{{add, -70, resistable}, {state, hp, def}}]}
            ]}
        ]},

        {roots, hunter, [
            {1, [
                { {{0, 1, casting}, []}, [{{set, 1, resistable}, {state, rem_moves, def}}]}
            ]}
        ]},

        {tree_hide, hunter, [
            {1, [
                { {{0, 3, casting}, []}, [{{add, 0.5, resistable}, {attr, off, armor}}]}
            ]}
        ]}
    ],

    Rogue = [
        {healing_potion, rogue, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, {175, 255}, resistable}, {state, hp, off}}]}
            ]}
        ]},

        {pierce_armor, rogue, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, -50, resistable}, {attr, armor, def}}]}
            ]}
        ]},

        {flurry, rogue, [
            {1, [
                { {{0, 1, casting}, []}, [{{set, 3, none}, {state, rem_moves, off}}]}
            ]}
        ]},

        {spellbreak, rogue, [
            {1, [
                { {{0, 4, casting}, []}, [{{add_mul, 0.7, none}, {attr, resist, off}}]}
            ]}
        ]},

        {perfect_strike, rogue, [
            {1, [
                { {{0, 1, casting}, []}, [
                {{set, 0, none}, {attr, dodge, def}},
                {{set, 0, none}, {attr, block, def}},
                {{set, 0, none}, {attr, hit_bonus, off}},
                {{set, 100, none}, {attr, critical, off}}]}
            ]}
        ]}
    ],

    Mage = [
        {vampiric_bolt, mage, [
            {1, [
                { {{0, 1, casting}, []}, [{{add_inc_mul, {{state, hp, def}, -0.1}, resistable}, {hp, off, none}}]},
                { {{0, 1, casting}, []}, [{{add_mul, 0.1, resistable}, {state, hp, def}}]}
            ]}
        ]},

        {arcane_surge, mage, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, 2, none}, {state, rem_moves, off}}]}
            ]}
        ]},

        {lower_resist, mage, [
            {1, [
                { {{0, 1, casting}, []}, 
                  [{{set, true, none}, {attr, attack_disabled, off}},
                   {{set, true, none}, {attr, cast_disabled, off}},
                   {{set, 0, none}, {attr, dodge, off}},
                   {{set, 0, none}, {attr, off, block}}
                  ]}
            ]}
        ]},

        {pyromania, mage, [
            {1, [
                { {{0, 3, casting}, []}, [{{add, -50, resistable}, {state, hp, def}}]},
                { {{0, 1, casting}, []}, [{{add_mul, -0.5, resistable}, {attr, critical, def}}]}
            ]}
        ]},

        {mind_blast, mage, [
            {1, [
                { {{0, 1, casting}, []}, [{{add, -125, resistable}, {state, hp, def}}]},
                { {{0, 1, casting}, []}, [{{set, 1, resistable}, {state, rem_moves, def}}]}
            ]}
        ]}
    ],

    ets:new(casts, [set, public, named_table]),

    ets:insert(casts, Talents),
    ets:insert(casts, CastsGeneral),
    ets:insert(casts, Mage),
    ets:insert(casts, Rogue),
    ets:insert(casts, Hunter),
    ets:insert(casts, Warrior).

