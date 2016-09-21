-module(battle).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

-import(effect, [effect/2]).


% ======================= MAIN BATTLE LOOP ============================
% Entrance, providing a battle context.

update_log(#{curr_hand:={_, {_, AtkType}, _}}=Attack, Defense, Battle)  ->
    
    {[
        { seq, maps:get(seq_no, Battle) }, { attacker, maps:get(id,Attack) },
        { defenser, maps:get(id, Defense)},
        { effect_name, maps:get(effect_name, Battle)},
        { attack_type, AtkType},
        { action, maps:get(outcome, Battle) },
        { def_damage, maps:get(def_damage, Battle) },
        { attacker_hp, maps:get(hp, Attack) },
        { defenser_hp, maps:get(hp, Defense) }
    ]}.


battle_loop(#{agility := A1} = P1, #{agility := A2} = P2) ->

    erlang:display('start new game'),
    % TODO: SHOULD BE MOVED TO AN ETS TABLE IN THE FUTURE.

    B = #{
        seq_no => 1,
        
        def_damage => 0,  % damage taken by defenser
        atk_damage => 0,  % damage taken by offenser
        
        is_defence_done => false,
        rem_atk => 2,
        effect_name => null,
        effect_action_list => EffectList,

        status => []
    },

    case A1 > A2 of
        true -> battle_loop(P1, P2, B, []);
        _    -> battle_loop(P2, P1, B, [])
    end.

% Condition of terminating: someone's HP dropped below zero.
battle_loop(#{hp:=AH, id:=AI}, #{hp:=DH, id:=DI}, _, Log) when AH < 0 orelse DH < 0 ->

    Winner = if
        AH < 0 -> DI;
        DH < 0 -> AI
    end,

    {done, jiffy:encode({[
        {proc, lists:reverse(Log)}, {res, Winner}
    ]} )};


battle_loop(
    #{agility:=AG}=A, #{agility:=DG}=D,
    #{is_defence_done:=true, rem_atk:=0, seq_no:=SeqNo}=B, L
) ->

    erlang:display('start new round'),
    
    NewB = B#{ is_defence_done := false, rem_atk := 2, seq_no := SeqNo + 1},

    case AG > DG of
        true -> battle_loop(A, D, NewB, L);
        _    -> battle_loop(D, A, NewB, L)
    end;


battle_loop(A, D, #{rem_atk:=0}=B, L) ->

    erlang:display('swap in same round'),

    battle_loop(D, A, B#{is_defence_done:=true, rem_atk:=2}, L);

% -------------- MAIN UNCONDITIONAL LOOP FOR BATTLE -------------------

battle_loop(A, D, B, L) ->

%    {PreA, PreD, PreB} = effect:apply_effects({A, D, B}),

%    LogAfterPre = [update_log(PreA, PreD, PreB) | L],

    {MovedA, MovedD, MovedB} = attacks:plain_attack({A, D, B}),

    LogAfterAttack = [update_log(MovedA, MovedD, MovedB) | L],

%    {PostA, PostD, PostB} = effect:apply_effects({MovedA, MovedD, MovedB}),

    battle_loop(MovedA, MovedD, MovedB, LogAfterAttack).

init_new_battle(Data) ->

    {P1, P2} = parse:player_context_from_parsed_JSON(Data), 
    erlang:display('start.'),
    battle_loop(P1#{status => [plain_attack]}, P2#{status => [plain_attack]}).
