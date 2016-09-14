-module(battle).

-author('Yue Marvin Tao').


-export([init_database/0, init_new_battle/1 ]).


get_player_weapon_type(#{curr_hand:=CurrHand, prim_type:=Prim, secd_type:=Secd}) ->

    case CurrHand of
        prim -> Prim;
        secd -> Secd
    end.



rotate(Roulette) ->

    % [2, 2, 2, 2, 2] -> [10, 8, 6, 4, 2, 0]
    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = random:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, Cumulative))),
    lists:nth(ResultIndex, [block, resist, dodge, critic, attack]).

% Preparing the roulette, might be affected by the buff or other conditions
% specified in Battle Context
prepare_roulette_from(
    #{resist:=Res, hit:=Hit, critic:=Critic}=A,
    #{secd_type:=Secd, block:=Blo, dodge:=Dod}, _B
) ->

    BasicAttack = 8, 

    {Dodge, Resist, Block} = case {get_player_weapon_type(A), Secd} of
        
        {{_, magic}, _} ->
            {0, Res, 0};
        
        {{_, physical}, {_, shield}} ->
            {Dod, 0, Blo};

        {{_, physical}, _} ->
            {Dod, 0, 0};

        % For the condition that supposed not to happen. should be avoided
        % before rotating the roulette.
        {_, _} ->
            {0, 0, 0}
    end,

    [BasicAttack + Hit, Critic, Dodge, Resist, Block].

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
        maps:get(curr_atk, Attack),
        maps:get(armor, Defense),
        Outcome
     ).

% ================ CREATE LOG ENTRY FOR CURRENT  ======================

update_log(Attack, Defense, Battle)  ->
    
    {[
        { seq, maps:get(seq_no, Battle) }, { attacker, maps:get(id,Attack) },
        { defenser, maps:get(id, Attack)},
        { attack_type, element(2, get_player_weapon_type(Attack)) },
        { action, maps:get(outcome, Battle) },
        { damage, maps:get(damage, Battle) },
        { attacker_hp, maps:get(hp, Attack) },
        { defenser_hp, maps:get(hp, Defense) }
    ]}.

% ================= GET PLAYER CONTEXT FROM CHAR  ======================

% In the future, both PlayerID and Char information should be extracted
% from player database.


% ======================= MAIN BATTLE LOOP ============================

% 主循环入口
battle_loop(#{agility := A1} = P1, #{agility := A2} = P2) ->

    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),

    B = #{
        seq_no => 1,
        damage => 0,
        is_latter => false,
        rem_atk => 2
    },

    case A1 > A2 of
        true ->
            battle_loop(P1, P2, B, []);
        _ ->
            battle_loop(P2, P1, B, [])
    end.



% 有一方血量不足，终止
battle_loop(#{hp:=AH, id:=AI}, #{hp:=DH, id:=DI}, _, Log) when AH < 0 orelse DH < 0 ->

    Winner = if
        AH < 0 -> DI;
        DH < 0 -> AI
    end,

    {done, jiffy:encode({[{proc, lists:reverse(Log)}, {res, Winner}] } )};

% 后手玩家剩余攻击次数用尽时，注意剩余攻击次数重置为2，但是未来会有更复杂的计算方法，
% 届时可以将此处的设定去掉，在无条件循环中依据buff状态等计算下一回合的剩余攻击次数。

battle_loop(
    #{agility:=AG}=A,
    #{agility:=DG}=D,
    #{is_latter:=true, rem_atk:=0, seq_no:=SeqNo}=B, L
) ->

    case AG > DG of
        true ->
            battle_loop(A, D, B#{is_latter:=false, rem_atk:=2, seq_no := SeqNo+1}, L);
        _ ->
            battle_loop(D, A, B#{is_latter:=false, rem_atk:=2, seq_no := SeqNo+1}, L)
    end;

% 排除以上情况，便是先手玩家剩余攻击次数用尽时

battle_loop(A, D, #{rem_atk:=0}=B, L) ->
    battle_loop(D, A, B#{is_latter:=true, rem_atk:=2}, L);

% -------------- MAIN UNCONDITIONAL LOOP FOR BATTLE -------------------
% 当以上的状况都没发生的时候，进入正常发起攻击的动作

battle_loop(Attack, Defense, #{rem_atk:=RemAtk}=Battle, Log) ->

    case get_player_weapon_type(Attack) of
        {no_damage, _} ->

            NextAttack = Attack#{
                curr_atk := maps:get(prim_range, Attack), 
                curr_hand := prim
            },
            NextBattle = Battle#{
                rem_atk := RemAtk - 1
            },
            battle_loop(NextAttack, Defense, NextBattle, Log);

        _ ->

            Outcome = rotate(prepare_roulette_from(Attack, Defense, Battle)),

            Damage = single_attack(Attack, Defense, Outcome),

            NextBattle = Battle#{
                outcome => Outcome,
                rem_atk => RemAtk - 1,
                damage => Damage
            },

            { NextHand, NextAttackRange} = case maps:get(curr_hand, Attack) of
                prim ->
                    {secd, maps:get(secd_range, Attack)};
                _ ->
                    {prim, maps:get(prim_range, Attack)}
            end,

            NextAttack = Attack#{
                curr_atk := NextAttackRange,
                curr_hand := NextHand 
            },
               
            NextDefense = Defense#{
                hp := maps:get(hp, Defense) - maps:get(damage, NextBattle)
            },

            % NextLog always records the Attacker's context before updated, and
            % Defenser's context after updated.
            NextLog = update_log(Attack, NextDefense, NextBattle),

            battle_loop(NextAttack, NextDefense, NextBattle, [NextLog | Log])

    end.

init_database() ->
    schema:rebuild_schema(),
    schema:rebuild_table().



init_new_battle(Data) ->

    {P1, P2} = parse:player_context_from_parsed_JSON(Data),
    battle_loop(P1, P2).
