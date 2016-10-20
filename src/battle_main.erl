-module(battle_main).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

% casting doesn't make any direct damage or other effects. Logging
% casting just for the convenience of playing animation.


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{id:=A, curr_attr:=#{agility:=AgiA}},
     #{id:=B, curr_attr:=#{agility:=AgiB}}) ->
    case rand:uniform() * (AgiA + AgiB) > AgiA of
        true -> B;
        _    -> A
    end.

% ----------- HELPER FUNCTION FOR SWAPPING OFFENDER/DEFENDER -----------

swap(Mover, #{id:=Mover}, #{id:=B}) -> B;
swap(Mover, #{id:=A}, #{id:=Mover}) -> A.


trans(Action, S={_, _, Mover}, #{id:=I1}=P1, #{id:=I2}=P2, L) ->
    
    case Mover of
        I1 -> Action(S, P1, P2, L);
        I2 -> {NewP2, NewP1, NewLog} = Action(S, P2, P1, L), {NewP1, NewP2, NewLog}
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

loop({Seq, attacking, _Mover},
     #{rem_moves:=0, prim_hand:=PrimHand1, orig_attr:=Orig1}=P1,
     #{rem_moves:=0, prim_hand:=PrimHand2, orig_attr:=Orig2}=P2,
     L) ->

    NewP1 = P1#{rem_moves:=2, curr_hand:=PrimHand1, curr_attr=>Orig1},
    NewP2 = P2#{rem_moves:=2, curr_hand:=PrimHand2, curr_attr=>Orig2},

    loop({Seq+1, settling, toss(NewP1, NewP2)}, NewP1, NewP2, L);


% ---------------- SWAPPING OFFENDER AND DEFENDER --------------------

% if not the case above, first we need to determine the guy currently
% moving is offender or defender. If the offender is moving, then we
% simply change to defender without changing anything else. If defender,
% we need to switch to the next phase.

loop({Seq, Stage, Mover}, #{rem_moves:=0}=P1, #{rem_moves:=0}=P2, L) ->

    NewStage = case Stage of
        settling -> casting;
        casting -> attacking
    end,
    
    loop({Seq, NewStage, swap(Mover, P1, P2)}, P1#{rem_moves:=2}, P2#{rem_moves:=2}, L);
    
loop({Seq, Stage, Mover}, #{id:=Mover, rem_moves:=0}=P1, P2, L) ->
    loop({Seq, Stage, swap(Mover, P1, P2)}, P1, P2, L);

loop({Seq, Stage, Mover}, P1, #{id:=Mover, rem_moves:=0}=P2, L) ->
    loop({Seq, Stage, swap(Mover, P1, P2)}, P1, P2, L);



% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

loop(S={_, attacking, _}, A, B, L) ->

    {AttackA, AttackB, AttackLog} = trans(fun(State, O, D, Log) ->
        {MovedO, MovedD, MovedLog} = battle_attack:attack(State, O, D, Log),
        battle_effect:effect(State, MovedO, MovedD, MovedLog)
    end, S, A, B, L),

    loop(S, AttackA, AttackB, AttackLog);


% ------------------------- LOOP FOR CAST -----------------------------

loop(S={_, casting, _}, A, B, L) ->

    {CastA, CastB, CastLog} = trans(fun(State, O, D, Log) ->
        
        {MovedO, MovedD, MovedLog} = battle_cast:cast(State, O, D, Log),
        battle_effect:effect(State, MovedO, MovedD, MovedLog)

    end, S, A, B, L),

    loop(S, CastA, CastB, CastLog);


% ---------------------- LOOP FOR SETTLEMENT -----------------------------

loop(S={_, settling, _}, A, B, L) ->

    {SettleO, SettleD, SettleLog} = trans(fun(State, O, D, Log) ->

        battle_effect:effect(State, O#{rem_moves:=0}, D, Log)

    end, S, A, B, L),

    loop(S, SettleO, SettleD, SettleLog).



init_new_battle(Data) ->

    {P1, P2} = battle_parse:player_context_from_parsed_JSON(Data),

    loop({0, attacking, maps:get(id, P1)}, P1, P2, []). 
