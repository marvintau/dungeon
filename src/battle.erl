-module(battle).

-author('Yue Marvin Tao').

-include("battle.hrl").

-export([init_database/0, init_new_battle/1 ]).


get_player_weapon_type(A) ->

    CurrHand = A#player.curr_hand,

    case CurrHand of
        prim ->
            A#player.prim_type;
        secd ->
            A#player.secd_type
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

    {Dodge, Resist, Block} = case {get_player_weapon_type(A), D#player.secd_type} of
        
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

magic_damage(Random, Outcome, {Lower, Upper}) ->
    
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

physical_damage(Random, Outcome, Armor, {Lower, Upper}) ->
    
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

single_attack(WeaponType, AtkRange, Armor, Outcome) ->

    Random = random:uniform(),

    case WeaponType of

        {_, magic} ->
            magic_damage(Random, Outcome, AtkRange);
        {_, physical} ->
            physical_damage(Random, Outcome, Armor, AtkRange);
        _ ->
            0
    end.

% ============ SINGLE ATTACK CALCULATED FROM CONTEXT ==================

single_attack(Attack, Defense, Outcome) ->

    single_attack(
        get_player_weapon_type(Attack),
        Attack#player.curr_atk,
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
    
    {[
        { seq, Battle#battle.seq_no }, { attacker, Attack#player.id },
        { attack_type, element(2, get_player_weapon_type(Attack)) },
        { action, Battle#battle.outcome },
        { damage, Battle#battle.damage },
        { attacker_hp, Attack#player.hp },
        { defenser_hp, Defense#player.hp }
    ]}.

% ================= GET PLAYER CONTEXT FROM CHAR  ======================

% In the future, both PlayerID and Char information should be extracted
% from player database.

player_context_from_char(PlayerID, Char) ->
    #player{
        id         = PlayerID,
        hp         = Char#char.hp,
        prim_type  = Char#char.prim_type,
        prim_range = Char#char.prim_range,
        secd_type  = Char#char.secd_type,
        secd_range = Char#char.secd_range,
        
        armor      = Char#char.armor,
        hit        = Char#char.hit,
        critic     = Char#char.critic,
        dodge      = Char#char.dodge,
        resist     = Char#char.resist,
        block      = Char#char.block,
        agility    = Char#char.agility,
 
        curr_hand  = prim,
        curr_atk   = Char#char.prim_range
    }.


% ======================= MAIN BATTLE LOOP ============================

% 主循环入口
battle_loop({Player1ID, Char1}, {Player2ID, Char2}, LogType) ->

    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),

    P1 = player_context_from_char(Player1ID, Char1),
    P2 = player_context_from_char(Player2ID, Char2),

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
battle_loop(A, D, _Battle, Log) when A#player.hp < 0 orelse D#player.hp < 0 ->

    Winner = if
        A#player.hp < 0 -> D#player.id;
        D#player.hp < 0 -> A#player.id
    end,

    {done, jiffy:encode({ [{proc, lists:reverse(Log)}, {res, Winner}] } )};

% 后手玩家剩余攻击次数用尽时，注意剩余攻击次数重置为2，但是未来会有更复杂的计算方法，
% 届时可以将此处的设定去掉，在无条件循环中依据buff状态等计算下一回合的剩余攻击次数。

battle_loop(A, D, B, L) when (B#battle.is_latter==true) and (B#battle.rem_atk==0)->

    SeqNo = B#battle.seq_no,

    case A#player.agility > D#player.agility of
        true ->
            battle_loop(A, D, B#battle{is_latter=false, rem_atk=2, seq_no = SeqNo+1}, L);
        _ ->
            battle_loop(D, A, B#battle{is_latter=false, rem_atk=2, seq_no = SeqNo+1}, L)
    end;

% 排除以上情况，便是先手玩家剩余攻击次数用尽时

battle_loop(A, D, B, L) when B#battle.rem_atk==0 ->
    battle_loop(D, A, B#battle{is_latter=true, rem_atk=2}, L);

% -------------- MAIN UNCONDITIONAL LOOP FOR BATTLE -------------------
% 当以上的状况都没发生的时候，进入正常发起攻击的动作

battle_loop(Attack, Defense, Battle, Log) ->

    case get_player_weapon_type(Attack) of
        {no_damage, _} ->

            NextAttack = Attack#player{
                curr_atk = Attack#player.prim_range, 
                curr_hand = prim
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

            { NextHand, NextAttackRange} = case Attack#player.curr_hand of
                prim ->
                    {secd, Attack#player.secd_range};
                _ ->
                    {prim, Attack#player.prim_range}
            end,

            NextAttack = Attack#player{
                curr_atk = NextAttackRange,
                curr_hand = NextHand 
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
