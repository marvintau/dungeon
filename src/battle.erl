-module(battle).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

-import(effect, [effect/2]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

update_log(#{curr_hand:={Which, {_, AtkType}, _}}=Attack, Defense, Battle)  ->
    
    {[
        { seq, maps:get(seq_no, Battle) }, { attacker, maps:get(id,Attack) },
        { defenser, maps:get(id, Defense)},
        %{ effect_name, maps:get(effect_name, Battle)},
        { which_hand, Which},
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

    B = #{
        seq_no => 0,
        
        % plain attack control
        outcome => null,
        def_damage => 0,  % damage taken by defenser
        atk_damage => 0,  % damage taken by offenser
        
        % for round control
        offenser => maps:get(id, P1),
        defenser => maps:get(id, P2),
        
        % force to re-calculate who is offensive, and
        % make sure the first round is casting.
        round_control => {attack, to_recalc},
        remaining_attacks => 0,
        
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

swap(_, _, #{offenser:= Off, defenser:=Def, round_control:= {MoveType, to_swap}}=B) ->
    B#{offenser:=Def, defenser:=Off, round_control:= {MoveType, to_recalc}};

swap(#{id:=I1, agility:=A1}, #{id:=I2, agility:=A2}, #{seq_no:=SeqNo, round_control:={MoveType, to_recalc}}=B) ->
    
    {Off, Def} = case A1 > A2 of
        true -> {I1, I2};
        _    -> {I2, I1}
    end,

    NewMoveType = case MoveType of
        attack -> cast;
        cast -> attack
    end,

    B#{offenser:=Off, defenser:=Def, round_control:={NewMoveType, to_swap}, seq_no:=SeqNo+1}.

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


% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

battle_loop(P1, P2, #{round_control:={attack, _}}=B, L) ->

    erlang:display({maps:get(offenser, B), make_plain_attack}),

    % for player's context, only the change of HP and the weapon on
    % current hand is saved for the next turn. All the attributes will
    % be restored before going to the next turn.
    {MovedP1, MovedP2, MovedB} = attack:plain_attack(P1, P2, B),
    #{hp:=NewHP1, curr_hand:=NewCurrHand1} = MovedP1,
    #{hp:=NewHP2, curr_hand:=NewCurrHand2} = MovedP2,

    LogAfterAttack = [update_log(P1, MovedP2, MovedB) | L],

    battle_loop(P1#{hp:=NewHP1, curr_hand:=NewCurrHand1},
                P2#{hp:=NewHP2, curr_hand:=NewCurrHand2},
                MovedB, LogAfterAttack);


% ------------------------- LOOP FOR CAST -----------------------------

battle_loop(P1, P2, #{round_control:={cast, _}}=B, L) ->

    erlang:display({maps:get(offenser, B), cast}),

    CastedB = cast:cast(P1, P2, B),

    battle_loop(P1, P2, CastedB#{remaining_attacks:=0}, L).


init_new_battle(Data) ->

    {P1, P2} = parse:player_context_from_parsed_JSON(Data), 
    battle_loop(P1, P2).
