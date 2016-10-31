-module(battle_db).

-author('Yue Marvin Tao').

-export([init_table/0, create_casts/0, list_casts/0, list_casts/1, list_cast_json/1]).

-export([update_cast/1]).

-export([remove_cast/1]).

init_table() ->
    create_casts().

parse_role_to_json({role, What, Whom, Attr}) ->
    {[{what, What}, {whom, Whom}, {attr, Attr}]}.

parse_range_to_json({Min, Max}) ->
    {[{type, range}, {value, {[{min, Min}, {max, Max}]} } ]};
parse_range_to_json(Value) ->
    {[{type, value}, {value, Value}]}.


parse_op_to_json({linear, {role, FromWhat, FromWhom, FromAttr}, Ratio}) ->
    {[{type, linear}, {ratio, Ratio}, {from, parse_role_to_json({role, FromWhat, FromWhom, FromAttr})}]};

parse_op_to_json({linear, IncRange, Ratio}) ->
    {[{type, linear}, {ratio, Ratio}, {from, parse_range_to_json(IncRange)}]};

parse_op_to_json({Op, IncRange}) ->
    {[{type, Op}, {from, parse_range_to_json(IncRange)}]}.

parse_trans_to_json({TransType, Op, To}) ->
    {[{type, TransType}, {op, parse_op_to_json(Op)}, {to, parse_role_to_json(To)}]}.

parse_single_effect_to_json(Effect) ->
    {Name, {Start, LastFor, Stage}, Trans, PossibleReact} = Effect,

    RoundCondition = {[{start, Start}, {last_for, LastFor}, {stage, Stage}]},

    {[{name, Name}, {round_cond, RoundCondition}, {trans, parse_trans_to_json(Trans)}, {react, PossibleReact}]}.

parse_single_group_to_json({Prob, Effects}) ->
    {[{prob, Prob}, {effects, [parse_single_effect_to_json(Effect) || Effect <- Effects]}]}.

parse_single_cast_to_json(Cast) ->
    {Name, Class, Groups} = Cast,

    {[{name, Name}, {class, Class}, {groups, [parse_single_group_to_json(Group) || Group <- Groups]}]}.

parse_casts_to_json(Casts) ->
    jiffy:encode([parse_single_cast_to_json(Cast) || Cast <- Casts]).

list_casts() ->
    AllCasts = lists:flatten(ets:match(casts, '$1')), 
    {done, parse_casts_to_json(AllCasts)}.

list_casts(Class) ->
    General = lists:flatten(ets:match(casts, {'$1', general, '_'})),
    ClassCast = lists:flatten(ets:match(casts, {'$1', Class, '_'})),
    lists:append(General, ClassCast).

list_cast_json(Data) ->
    {[{<<"id">>, _ID}, {<<"class">>, Class}]} = Data,
    ReturnedData = [ none | list_casts(binary_to_atom(Class, utf8))],    
    error_logger:info_report(ReturnedData),
    {done, jiffy:encode(ReturnedData)}.


parse_range({[{_, <<"range">>}, {_, {[{_, Min}, {_, Max}]} } ]}) ->
    {Min, Max};

parse_range({[_, {_, Value}]}) ->
    if 
        is_number(Value) or is_boolean(Value) -> Value;
        is_binary(Value) -> binary_to_atom(Value, utf8)
    end.

parse_role({[{_, What}, {_, Whom}, {_, Attr}]}) ->
    {role, binary_to_atom(What, utf8), binary_to_atom(Whom, utf8), binary_to_atom(Attr, utf8)}.

parse_op(direct, {[{_, Type}, {_, From}]}) ->
    {binary_to_atom(Type, utf8), parse_range(From)};
parse_op(indirect, {[{_, Type}, {_, Ratio}, {_, Role}]}) ->
    {binary_to_atom(Type, utf8), Ratio, parse_role(Role)}.

parse_trans({[{_, <<"direct">>}, {_, Op}, {_, To}]}) ->
    {direct, parse_op(direct, Op), parse_role(To)};
parse_trans({[{_, <<"indirect">>}, {_, Op}, {_, To}]}) ->
    {indirect, parse_op(indirect, Op), parse_role(To)}.

parse_cond({[{_, Start}, {_, Last}, {_, Stage}]}) ->
    {Start, Last, binary_to_atom(Stage, utf8)}.

parse_single_effect({[{_, Name}, {_, Cond}, {_, Trans}, {_, React}]}) ->
    {binary_to_atom(Name, utf8), parse_cond(Cond), parse_trans(Trans), binary_to_atom(React, utf8)}.

parse_effects(Effects) ->
    [parse_single_effect(Effect) || Effect <- Effects].

parse_single_group({[{_, Prob}, {_, Effects}]}) ->
    {Prob, parse_effects(Effects)}.

parse_groups(Groups) ->
   [parse_single_group(Group) || Group <- Groups]. 

parse_cast(CastData) ->
    {[{_, Name}, {_, Class}, {_, Groups}]} = CastData,

    {binary_to_atom(Name, utf8), binary_to_atom(Class, utf8), parse_groups(Groups)}. 


update_cast(Data) ->
    Decoded = jiffy:decode(Data),
    {Name, _, _} = Res = parse_cast(Decoded),
    error_logger:info_report(ets:lookup(casts, Name)),
    ets:insert(casts, Res),
    error_logger:info_report(ets:lookup(casts, Name)),
    ok.

remove_cast(Data) ->
    Decoded = jiffy:decode(Data),
    {Name, _, _} = parse_cast(Decoded),
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
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_opponent, hit}}, none},
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
                {first_aid, {0, 1, casting}, {direct, {times, -0.08}, {role, hp, of_self, none}}, none}
            ]}
        ]}
    ],


    Hunter = [
        {tornado, hunter, [
            {1, [
                {tornado, {0, 1, casting}, {direct, {times, -0.05}, {role, attr, of_opponent, hit}}, none},          
                {tornado, {1, 4, settling}, {direct, {times, -0.05}, {role, attr, of_opponent, hit}}, none},          
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
                {shield_wall, {0, 1, casting}, {direct, {set, 0}, {role, attr, of_self, hit}}, none},
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


