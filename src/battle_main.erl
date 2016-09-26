-module(battle_main).

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

loop(P1, P2) ->

    B = #{
        seq_no => 0,
        
        % plain attack control
        outcome => null,
        def_damage => 0,  % damage taken by defenser
        atk_damage => 0,  % damage taken by offender
        
        % for round control
        offender => maps:get(id, P1),
        defenser => maps:get(id, P2),
        
        % force to re-calculate who is offensive, and
        % make sure the first round is casting.
        round_control => {attacking, to_recalc},
        remaining_attacks => 0,
        
        effects => [] 
    },

    loop(P1, P2, B, []).



% -------------------------- ROUND CONTROL ----------------------------
% if the swapping mode is marked as to_recalc, which means it's on the
% defender's move, and the current remaining attack is zero, not only
% we need to recalculate the next offensive person, but also need to 
% change the move type to casting.

% If one player consumes all his remainng attacks, the first loop will
% be executed, with remaining attacks reset. Notably, the new remaining
% attacks might be any number above 0, and should be reasonably reset
% within the stuff. It doesn't take care of which player  

swap(_, _, #{offender:= Off, defenser:=Def, round_control:= {MoveType, to_swap}}=B) ->

    B#{offender:=Def, defenser:=Off, round_control:= {MoveType, to_recalc}};

swap(#{id:=I1, agility:=A1}, #{id:=I2, agility:=A2}, #{round_control:={_MoveType, to_recalc}}=B) ->
    
    {NewOff, NewDef} = case A1 > A2 of
        true -> {I1, I2};
        _    -> {I2, I1}
    end,

    B#{offender:=NewOff, defenser:=NewDef, round_control:={_MoveType, to_swap}}.


loop(P1, P2, #{remaining_attacks:=0}=B, L) ->
    SwappedB = swap(P1, P2, B),
    loop(P1, P2, SwappedB#{remaining_attacks:=2}, L);

% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(#{hp:=HP1, id:=I1}, #{hp:=HP2, id:=I2}, _, Log) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if
        HP1 < 0 -> I2;
        HP2 < 0 -> I1
    end,

    {done, jiffy:encode({[
        {proc, lists:reverse(Log)}, {res, Winner}
    ]} )};


% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

loop(P1, P2, #{round_control:={attacking, _}}=B, L) ->

    erlang:display({maps:get(offender, B), make_plain_attack}),

    % for player's context, only the change of HP and the weapon on
    % current hand is saved for the next turn. All the attributes will
    % be restored before going to the next turn.
    {MovedP1, MovedP2, MovedB} = battle_attack:plain_attack(P1, P2, B),
    #{hp:=NewHP1, curr_hand:=NewCurrHand1} = MovedP1,
    #{hp:=NewHP2, curr_hand:=NewCurrHand2} = MovedP2,

    LogAfterAttack = [update_log(P1, MovedP2, MovedB) | L],

    loop(P1#{hp:=NewHP1, curr_hand:=NewCurrHand1},
                P2#{hp:=NewHP2, curr_hand:=NewCurrHand2},
                MovedB, LogAfterAttack);


% ------------------------- LOOP FOR CAST -----------------------------

loop(P1, P2, #{round_control:={casting, _}}=B, L) ->

    erlang:display({maps:get(offender, B), cast}),

    CastedB = battle_cast:cast(P1, P2, B),

    loop(P1, P2, CastedB#{remaining_attacks:=0}, L).


init_new_battle(Data) ->

    erlang:display(yay),

    {P1, P2} = battle_parse:player_context_from_parsed_JSON(Data), 
    loop(P1, P2).
