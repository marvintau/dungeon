-module(battle).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

-import(effect, [effect/2]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

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



% ======================= MAIN BATTLE LOOP ============================

% ------------------------- ENTRANCE --------------------------------
% the outer loop doesn't distinguish the offensive and defensive. The
% entrance of loop will generate the battle context. 

battle_loop(P1, P2) ->

    erlang:display('New battle started.'),

    B = #{
        seq_no => 0,
        
        def_damage => 0,  % damage taken by defenser
        atk_damage => 0,  % damage taken by offenser
        
        % for round control
        offenser => maps:get(id, P1),
        defenser => maps:get(id, P2),
        
        % force to re-calculate who is offensive
        round_control => to_recalc,
        remaining_attacks => 0,
        
    
        outcome => null,

        effect_name => null,
        effect_action_list => null,

        status => []
    },

    battle_loop(P1, P2, B, []).



% -------------------------- SWAP PLAYER ------------------------------
% if offenser has made move, then let the defenser make move. If both
% have made move, then calculate who is the next offenser in next round.

% If one player consumes all his remainng attacks, the first loop will
% be executed, with remaining attacks reset. Notably, the new remaining
% attacks might be any number above 0, and should be reasonably reset
% within the stuff. It doesn't take care of which player  

swap(_, _, #{offenser:= Off, defenser:=Def, round_control:=to_swap}=B) ->
    erlang:display(swapped),
    B#{offenser:=Def, defenser:=Off, round_control:=to_recalc};

swap(#{id:=I1, agility:=A1}, #{id:=I2, agility:=A2}, #{seq_no:=SeqNo, round_control:=to_recalc}=B) ->
    
    erlang:display(re_calculated),

    {Off, Def} = case A1 > A2 of
        true -> {I1, I2};
        _    -> {I2, I1}
    end,

    B#{offenser:=Off, defenser:=Def, round_control:=to_swap, seq_no:=SeqNo+1}.

% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

battle_loop(#{hp:=HP1, id:=I1}, #{hp:=HP2, id:=I2}, _, Log) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if
        HP1 < 0 -> I2;
        HP2 < 0 -> I1
    end,

    {done, jiffy:encode({[
        {proc, lists:reverse(Log)}, {res, Winner}
    ]} )};

% -------------------------- SWAP PLAYER ------------------------------
battle_loop(P1, P2, #{remaining_attacks:=0} = B, L) ->
    
    SwappedB = swap(P1, P2, B),
    battle_loop(P1, P2, SwappedB#{remaining_attacks:=2}, L);


% ------------------- LOOP BODY THAT DO STUFF  -----------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

battle_loop(P1, P2, B, L) ->

    erlang:display({new_move, maps:get(offenser, B)}),
    {MovedP1, MovedP2, MovedB} = attacks:plain_attack(P1, P2, B),
    LogAfterAttack = [update_log(MovedP1, MovedP2, MovedB) | L],

    battle_loop(MovedP1, MovedP2, MovedB, LogAfterAttack).

init_new_battle(Data) ->

    {P1, P2} = parse:player_context_from_parsed_JSON(Data), 
    erlang:display('start.'),
    battle_loop(P1, P2).
