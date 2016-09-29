-module(battle_main).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

update_log({Seq, Stage, Role, {Mover, _, Rem}, _},
           #{curr_attr:=#{outcome:=Outcome, damage_dealt:=Damage}=_, 
             curr_hand:={Which, {_, AtkType}, _}}=O, D)  ->
    
    {[
        { seq, Seq }, { offender, Mover }, {role, Role}, { defender, maps:get(id, D)},

        { hand, Which}, { attack_type, AtkType}, {rem_atks, Rem},
        { action, Outcome }, { damage, Damage },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{id:=I1, rem_attacks:=Rem1, curr_attr:=#{agility:=A1}},
     #{id:=I2, rem_attacks:=Rem2, curr_attr:=#{agility:=A2}}) ->
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
     erlang:display(" "),
    
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

loop(State={Seq, attacking, DefOff, {Mover, Hand, Rem}, Effects},
     #{id:=I1, hp:=H1, prim_hand:=Prim1, secd_hand:=Secd1, curr_hand:=Curr1, curr_attr:=Attr1}=P1,
     #{id:=I2, hp:=H2, prim_hand:=Prim2, secd_hand:=Secd2, curr_hand:=Curr2, curr_attr:=Attr2}=P2, L) ->

    erlang:display({attacking, State}),

    {Outcome, Damage} = case Mover of
        I1 -> battle_attack:get_final_damage(P1, P2);
        _  -> battle_attack:get_final_damage(P2, P1)
    end,

    erlang:display({attacked, Mover, {Curr1, Curr2}, {Outcome, Damage}}),

    {MovedP1, MovedP2} = case Mover of
        I1 -> { P1#{curr_attr:=Attr1#{damage_dealt:=Damage, outcome:=Outcome}},
                P2#{hp:=H2 - Damage}
        };

        _  -> { P1#{hp:=H1 - Damage},
                P2#{curr_attr:=Attr2#{damage_dealt:=Damage, outcome:=Outcome}}
        }
    end,

    NewLogEntry = case Mover of
        I1 -> update_log(State, MovedP1, MovedP2);
        I2 -> update_log(State, MovedP2, MovedP1)
    end,

    {NewHand1, NewHand2, NewCurrHand} = case {Mover, Hand} of
        {I1, prim} -> {Secd1, Curr2, secd};
        {I1, secd} -> {Prim1, Curr2, prim};
        {I2, prim} -> {Curr1, Secd2, secd};
        {I2, secd} -> {Curr1, Prim2, prim}
    end,

    % The effect of casting with regard to Outcome and Damage will be
    % added here.

    loop({Seq, attacking, DefOff, {Mover, NewCurrHand, Rem-1}, Effects},
        MovedP1#{curr_hand:= NewHand1}, MovedP2#{curr_hand:= NewHand2}, [NewLogEntry| L]);


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
