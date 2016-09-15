-module(battle).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).



% ======================= MAIN BATTLE LOOP ============================

% 主循环入口
battle_loop(#{agility := A1} = P1, #{agility := A2} = P2) ->

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

battle_loop(A, D, B, L) ->

    {NextAttack, NextDefense, NextBattle, NextLog} = attacks:plain_attack(A, D, B, L),
    battle_loop(NextAttack, NextDefense, NextBattle, NextLog).

init_new_battle(Data) ->

    {P1, P2} = parse:player_context_from_parsed_JSON(Data),
    battle_loop(P1, P2).
