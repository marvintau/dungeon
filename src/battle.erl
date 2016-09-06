-module(battle).

-author('Yue Marvin Tao').

-include("battle.hrl").

-export([init_database/0, init_new_battle/1 ]).


get_player_weapon_type(A) ->

    CurrHand = A#player.which_hand,

    case CurrHand of
        primary ->
            A#player.char#char.pri_type;
        secondary ->
            A#player.char#char.sec_type
    end.



rotate(Roulette) ->

    % [2, 2, 2, 2, 2] -> [10, 8, 6, 4, 2, 0]
    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = random:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, Cumulative))),
    lists:nth(ResultIndex, [block, resist, dodge, critic, attack]).

% Preparing the roulette, might be affected by the buff or other conditions
% specified in Battle Context
prepare_roulette_from(A, D, _B) ->

    BasicAttack = 8, 

    {Dodge, Resist, Block} = case {get_player_weapon_type(A), D#player.char#char.sec_type} of
        
        {{_, magic}, _} ->
            {0, A#player.resist, 0};
        
        {{_, physical}, {_, shield}} ->
            {D#player.dodge, 0, D#player.block};

        {{_, physical}, _} ->
            {D#player.dodge, 0, 0};

        % For the condition that supposed not to happen. should be avoided
        % before rotating the roulette.
        {_, _} ->
            {0, 0, 0}
    end,

    [BasicAttack + A#player.hit, A#player.critic, Dodge, Resist, Block].

magic_damage(Random, Outcome, Upper, Lower) ->
    
    % Magic attack cannot be blocked, thus make sure that block has
    % been set 0 before turning the roulette.

    case Outcome of
        dodge ->
            0;
        resist ->
            round(Random * Lower / 10);
        critic ->
            round((Lower + Random * (Upper - Lower)) * 2);
        _ ->
            round(Lower + Random * (Upper - Lower))
    end.

physical_damage(Random, Outcome, Upper, Lower, Armor) ->
    
    % --------------- PLAYER AS NON-MAGE --------------------------
    % Physical attack cannot be resisted, make sure that resist has
    % been ser 0 before turning the roulette.

    case Outcome of
        dodge ->
            0;
        block ->
            0;
        critic ->
            round((Lower + Random * (Upper - Lower)) * 2 * (1 - Armor * 0.0001));
        _ ->
            round(Lower + Random * (Upper - Lower) * (1 - Armor * 0.0001))
    end.


% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

% Calculates the damage with given character type, the upper and lower
% damage of weapon, and outcome of roulette turning.

single_attack(WeaponType, Upper, Lower, Armor, Outcome) ->

    Random = random:uniform(),

    case WeaponType of

        {_, magic} ->
            magic_damage(Random, Outcome, Upper, Lower);
        {_, physical} ->
            physical_damage(Random, Outcome, Upper, Lower, Armor);
        _ ->
            0
    end.

% ============ SINGLE ATTACK CALCULATED FROM CONTEXT ==================

single_attack(Attack, Defense, Outcome) ->

    single_attack(
        get_player_weapon_type(Attack),
        Attack#player.atk_upper,
        Attack#player.atk_lower,
        Defense#player.armor,
        Outcome
     ).

% ================ CREATE LOG ENTRY FOR CURRENT  ======================

join(Sep, Items) ->
        lists:flatten(lists:reverse(join1(Items, Sep, []))).
 
join1([Head | []], _Sep, Acc) ->
        [Head | Acc];
join1([Head | Tail], Sep, Acc) ->
        join1(Tail, Sep, [Sep, Head | Acc]).

update_log(Attack, Defense, Battle)  ->
    case Battle#battle.log_type of
        json ->
            join("", [ "{",
                "\"seq\":", integer_to_list(Battle#battle.seq_no), ", ",
                "\"attacker\": \"", atom_to_list(Attack#player.id), "\", ",
                "\"type\": \"", atom_to_list(Attack#player.char#char.type), "\", ",
                "\"attack_type\": \"", atom_to_list(element(2, get_player_weapon_type(Attack))), "\", ",
                "\"outcome\": \"", atom_to_list(Battle#battle.outcome), "\", ",
                "\"damage\":", integer_to_list(Battle#battle.damage), ", ",
                "\"attacker_hp\":", integer_to_list(Attack#player.hp), ", ",
                "\"defenser_hp\":", integer_to_list(Defense#player.hp), 
                "}"]
            );
        
        html ->
            list_to_binary(["<tr>",
            "<td>", integer_to_list(Battle#battle.seq_no), "</td>",
            "<td>", atom_to_list(Attack#player.id),"</td> ",
            "<td>", atom_to_list(element(2, get_player_weapon_type(Attack))), "</td>",
            "<td>", atom_to_list(Battle#battle.outcome), "</td>",
            "<td>", integer_to_list(Battle#battle.damage), "</td>",
            "<td>", integer_to_list(Attack#player.hp), "</td>",
            "<td>", integer_to_list(Defense#player.hp), "</td>"
            "</tr> "])
    end.

wrap_log(B, Log) ->
    case B#battle.log_type of
        json ->
            {done, Log};
        html ->
            {done, <<"<table>", Log/binary, "</table>">>}
    end.

% ================= GET PLAYER CONTEXT FROM CHAR  ======================

% In the future, both PlayerID and Char information should be extracted
% from player database.

player_from_char(PlayerID, Char) ->
    #player{
        id         = PlayerID,
        char       = Char,
        hp         = Char#char.hp,
        armor      = Char#char.armor,
        hit        = Char#char.hit,
        critic     = Char#char.critic,
        dodge      = Char#char.dodge,
        resist     = Char#char.resist,
        block      = Char#char.block,
        agility    = Char#char.agility,
 
        which_hand = primary,
        atk_upper  = element(2, Char#char.primary),
        atk_lower  = element(1, Char#char.primary)
    }.


% ======================= MAIN BATTLE LOOP ============================

% 主循环入口
battle_loop({Player1ID, Char1}, {Player2ID, Char2}, LogType) ->

    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),

    P1 = player_from_char(Player1ID, Char1),
    P2 = player_from_char(Player2ID, Char2),

    B = #battle{
        seq_no = 1,
        log_type = LogType,
        damage = 0,
        is_latter = false,
        rem_atk = 2
    },

    case P1#player.agility > P2#player.agility of
        true ->
            battle_loop(P1, P2, B, []);
        _ ->
            battle_loop(P2, P1, B, [])
    end.



% 有一方血量不足，终止
battle_loop(A, D, Battle, Log) when A#player.hp < 0 orelse D#player.hp < 0 ->
    
    NewLog = if
        A#player.hp < 0 ->
            list_to_binary([<<"{\"win\": \"">>, atom_to_list(D#player.id), <<"\"}">>]);
        D#player.hp < 0 ->
            list_to_binary([<<"{\"win\": \"">>, atom_to_list(A#player.id), <<"\"}">>])
    end,

    erlang:display(is_list(Log)),
    erlang:display(Log),
    wrap_log(Battle, <<"{\"proc\": [", (list_to_binary(join(",", lists:reverse(Log))))/binary, "], \"res\": ", NewLog/binary, "}">>);

% 后手玩家剩余攻击次数用尽时，注意剩余攻击次数重置为2，但是未来会有更复杂的计算方法，
% 届时可以将此处的设定去掉，在无条件循环中依据buff状态等计算下一回合的剩余攻击次数。

battle_loop(A, D, B, L) when (B#battle.is_latter==true) and (B#battle.rem_atk==0) ->

    SeqNo = B#battle.seq_no,

    case A#player.agility > D#player.agility of
        true ->
            battle_loop(A, D, B#battle{is_latter=false, rem_atk=2, seq_no = SeqNo+1}, L);
        _ ->
            battle_loop(D, A, B#battle{is_latter=false, rem_atk=2, seq_no = SeqNo+1}, L)
    end;

% 排除以上情况，便是先手玩家剩余攻击次数用尽时

battle_loop(A, D, B, L) when B#battle.rem_atk=:=0 ->
    battle_loop(D, A, B#battle{is_latter=true, rem_atk=2}, L);

% -------------- MAIN UNCONDITIONAL LOOP FOR BATTLE -------------------
% 当以上的状况都没发生的时候，进入正常发起攻击的动作

battle_loop(Attack, Defense, Battle, Log) ->

    case get_player_weapon_type(Attack) of
        {no_damage, bare} ->
            
            {NextAttackLower, NextAttackUpper} = Attack#player.char#char.primary,

            NextAttack = Attack#player{
                atk_lower = NextAttackLower,
                atk_upper = NextAttackUpper,
                which_hand = primary 
            },

            NextBattle = Battle#battle{
                rem_atk = Battle#battle.rem_atk - 1
            },
            battle_loop(NextAttack, Defense, NextBattle, Log);

        {no_damage, shield} ->

            {NextAttackLower, NextAttackUpper} = Attack#player.char#char.primary,

            NextAttack = Attack#player{
                atk_lower = NextAttackLower,
                atk_upper = NextAttackUpper,
                which_hand = primary
            },
            NextBattle = Battle#battle{
                rem_atk = Battle#battle.rem_atk - 1
            },
            battle_loop(NextAttack, Defense, NextBattle, Log);
        _ ->

            Outcome = rotate(prepare_roulette_from(Attack, Defense, Battle)),

            Damage = single_attack(Attack, Defense, Outcome),
         

            NextBattle = Battle#battle{
                outcome = Outcome,
                rem_atk = Battle#battle.rem_atk - 1,
                damage = Damage
            },

            { NextHand, {NextAttackUpper, NextAttackLower}} = case Attack#player.which_hand of
                primary ->
                    {secondary, Attack#player.char#char.secondary};
                _ ->
                    {primary, Attack#player.char#char.primary}
            end,

            NextAttack = Attack#player{
                atk_lower = NextAttackLower,
                atk_upper = NextAttackUpper,
                which_hand = NextHand
            },

               
            NextDefense = Defense#player{
                hp = Defense#player.hp - NextBattle#battle.damage 
            },

            % NextLog always records the Attacker's context before updated, and
            % Defenser's context after updated.
            NextLog = update_log(Attack, NextDefense, NextBattle),

            battle_loop(NextAttack, NextDefense, NextBattle, [NextLog | Log])

    end.



init_database() ->
    schema:rebuild_schema(),
    schema:rebuild_table().

random_char() ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    
    schema:get_char(lists:nth(trunc(random:uniform()*4)+1, [warrior, hunter, mage, rogue])).


init_new_battle(LogType) ->
    error_logger:info_report("Initiates new battle."),
    battle_loop({alice, random_char()}, {bob, random_char()}, LogType).
