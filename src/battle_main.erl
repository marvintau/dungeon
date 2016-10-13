-module(battle_main).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

% casting doesn't make any direct damage or other effects. Logging
% casting just for the convenience of playing animation.


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{id:=I1, rem_moves:=Rem1, curr_attr:=#{agility:=A1}},
     #{id:=I2, rem_moves:=Rem2, curr_attr:=#{agility:=A2}}) ->
    case rand:uniform() * (A1 + A2) > A1 of
        true -> {I2, prim, Rem2};
        _    -> {I1, prim, Rem1}
    end.

% ----------- HELPER FUNCTION FOR SWAPPING OFFENDER/DEFENDER -----------

swap(Mover, #{id:=I1, rem_moves:=Rem1}, #{id:=I2, rem_moves:=Rem2}) ->
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

loop({Seq, attacking, defensive, {_Mover, _Hand, 0}, Effects},
     #{prim_hand:=PrimHand1, orig_attr:=Orig1}=P1,
     #{prim_hand:=PrimHand2, orig_attr:=Orig2}=P2,
     L) ->

    loop({Seq+1, settling, offensive, toss(P1, P2), Effects},
         P1#{curr_hand:=PrimHand1, curr_attr:=Orig1},
         P2#{curr_hand:=PrimHand2, curr_attr:=Orig2},
         L);


% ---------------- SWAPPING OFFENDER AND DEFENDER --------------------

% if not the case above, first we need to determine the guy currently
% moving is offender or defender. If the offender is moving, then we
% simply change to defender without changing anything else. If defender,
% we need to switch to the next phase.

loop({Seq, MoveType, DefOff, {Mover, _Hand, 0}, Effects}, P1, P2, L) ->

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

loop(S={_, attacking, _, _, _}, P1, P2, L) ->

    {MovedState, MovedP1, MovedP2, MovedLog} = battle_attack:attack(S, P1, P2, L),

    {AffectedP1, AffectedP2, AffectedLog} = battle_effect:apply_effects(MovedState, MovedP1, MovedP2, MovedLog),

    loop(MovedState, AffectedP1, AffectedP2, AffectedLog);


% ------------------------- LOOP FOR CAST -----------------------------

loop(State={_, casting, _, _, _}, P1, P2, L) ->

    {NewState, CastedP1, CastedP2, LogCasted} = battle_cast:cast(State, P1, P2, L),

    {AffectedP1, AffectedP2, LogAffected} = battle_effect:apply_effects(NewState, CastedP1, CastedP2, LogCasted),

    loop(NewState, AffectedP1, AffectedP2, LogAffected);


% ---------------------- LOOP FOR SETTLEMENT -----------------------------

loop(State={Seq, settling, DefOff, {Mover, Hand,  _}, Effects}, P1, P2, L) ->

    {AffectedP1, AffectedP2, LogAffected} = battle_effect:apply_effects(State, P1, P2, L),
    
    loop({Seq, settling, DefOff, {Mover, Hand, 0}, Effects}, AffectedP1, AffectedP2, LogAffected).



init_new_battle(Data) ->

    battle_init:create_cast_table(),

    {P1, P2} = battle_parse:player_context_from_parsed_JSON(Data),

    loop({0, attacking, defensive, {maps:get(id, P1), prim, 0}, []}, P1, P2, []). 
