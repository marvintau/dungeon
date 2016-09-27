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


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{id:=I1, rem_attacks:=Rem1, curr_attr:=#{agility:=A1}}, #{id:=I2, rem_attacks:=Rem2, curr_attr:=#{agility:=A2}}) ->
    case rand:uniform() * (A1 + A2) > A1 of
        true -> {I2, prim, Rem2};
        _    -> {I1, prim, Rem1}
    end.

% ----------- HELPER FUNCTION FOR SWAPPING OFFENDER/DEFENDER -----------

swap(Mover, #{id:=I1, rem_attacks:=Rem1}, #{id:=I2, rem_attacks:=Rem2}) ->
    case Mover == I1 of
        true -> {I2, prim, Rem2};
        _    -> {I1, prim, Rem1}
    end.


% ======================= MAIN BATTLE LOOP ============================


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{hp:=HP1, id:=I1}, #{hp:=HP2, id:=I2}, Log) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if
        HP1 < 0 -> I2;
        HP2 < 0 -> I1
    end,

    {done, jiffy:encode({[
        {proc, lists:reverse(Log)}, {res, Winner}
    ]} )};


% ------------------ RECALCULATING NEW OFFENDER ----------------------
% after defender consumes last chance of attack, increase the sequence
% number, re-calculate the new offender with their agility, and assign
% the new number of remaining attacks.

% Moreover, since the player's attributes should be recalculated in the
% new round, the attributes should be restored in this stage. Meanwhile,
% both players will be restored to primary hand.

loop(State={Seq, attacking, defensive, {_Mover, _Hand, 0}, Effects},
     #{prim_hand:=PrimHand1, orig_attr:=Orig1}=P1,
     #{prim_hand:=PrimHand2, orig_attr:=Orig2}=P2,
     L) ->

    erlang:display({tossing, State}),
    
    loop({Seq+1, settling, offensive, toss(P1, P2), Effects},
         P1#{curr_hand:=PrimHand1, curr_attr:=Orig1},
         P2#{curr_hand:=PrimHand2, curr_attr:=Orig2},
         L);



% ---------------- SWAPPING OFFENDER AND DEFENDER --------------------

% if not the case above, first we need to determine the guy currently
% moving is offender or defender. If the offender is moving, then we
% simply change to defender without changing anything else. If defender,
% we need to switch to the next phase.

loop(State={Seq, MoveType, DefOff, {Mover, _Hand, 0}, Effects}, P1, P2, L) ->

    erlang:display({swapping, State}),

    NewDefOff = case DefOff of
        defensive -> offensive;
        _         -> defensive
    end,

    NewMoveType = case DefOff of
        offensive -> MoveType;
        _         ->
            case MoveType of
                settling -> casting;
                casting -> attacking
            end
    end,
    
    loop({Seq, NewMoveType, NewDefOff, swap(Mover, P1, P2), Effects}, P1, P2, L);
    

% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

loop(State={Seq, attacking, DefOff, {Mover, Hand, Rem}, Effects}, #{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2, L) ->

    erlang:display({attacking, State}),

    {Outcome, Damage} = case Mover of
        I1 -> battle_attack:get_final_damage(P1, P2);
        _  -> battle_attack:get_final_damage(P2, P1)
    end,

    erlang:display({Outcome, Damage}),

    {{Damage1, Damage2}, {NewHand1, NewHand2}, NewHand} = case {Mover, Hand} of
        {I1, prim} -> {{0, Damage}, {maps:get(secd_hand, P1), maps:get(curr_hand, P2)}, secd};
        {I1, secd} -> {{0, Damage}, {maps:get(prim_hand, P1), maps:get(curr_hand, P2)}, prim};
        {I2, prim} -> {{Damage, 0}, {maps:get(curr_hand, P1), maps:get(secd_hand, P2)}, secd};
        {I2, secd} -> {{Damage, 0}, {maps:get(curr_hand, P1), maps:get(secd_hand, P2)}, prim}
    end,

    % The effect of casting with regard to Outcome and Damage will be
    % added here.

    loop({Seq, attacking, DefOff, {Mover, NewHand, Rem-1}, Effects},
        P1#{hp:=H1 - Damage1, curr_hand:= NewHand1},
        P2#{hp:=H2 - Damage2, curr_hand:= NewHand2}, L);


% ------------------------- LOOP FOR CAST -----------------------------

loop(State={Seq, casting, DefOff, {Mover, Hand,  _}, Effects}, P1, P2, L) ->

    erlang:display({casting, State}),

    loop({Seq, casting, DefOff, {Mover, Hand, 0}, Effects}, P1, P2, L);


% ---------------------- LOOP FOR SETTLEMENT -----------------------------

loop(State={Seq, settling, DefOff, {Mover, Hand,  _}, Effects}, P1, P2, L) ->

    erlang:display({settling, State}),

    loop({Seq, settling, DefOff, {Mover, Hand, 0}, Effects}, P1, P2, L).




init_new_battle(Data) ->

    erlang:display({battle, starts}),

    {P1, P2} = battle_parse:player_context_from_parsed_JSON(Data),

    loop({0, attacking, defensive, {maps:get(id, P1), prim, 0}, []}, P1, P2, []). 
