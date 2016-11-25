-module(battle_main).

-author('Yue Marvin Tao').

-export([init_new_battle/1 ]).

% ------------------- HELPER FUNCTION FOR LOGGING ---------------------

% casting doesn't make any direct damage or other effects. Logging
% casting just for the convenience of playing animation.


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

% Any cast that might change the normal way of determine the order of
% attack will be put here.

toss(#{id:=A, casts:=[rune_of_the_void|_]}, _) -> 
    A;
toss(_, #{id:=B, casts:=[rune_of_the_void|_]}) -> 
    B;

toss(#{id:=A, attr:=#{agility:=AgiA}},
     #{id:=B, attr:=#{agility:=AgiB}}) ->
    case rand:uniform() * (AgiA + AgiB) > AgiA of
        true -> B;
        _    -> A
    end.

% ----------- HELPER FUNCTION FOR SWAPPING OFFENDER/DEFENDER -----------

swap(Mover, #{id:=Mover}, #{id:=B}) -> B;
swap(Mover, #{id:=A}, #{id:=Mover}) -> A.


trans(Action, #{mover:=Mover}=S, #{id:=I1}=P1, #{id:=I2}=P2, L) ->

    %erlang:display({I2, maps:get(rem_moves, maps:get(state, P2))}),

    {Res1, Res2, ResLog} = case Mover of
        I1 -> Action(S, P1, P2, L);
        I2 -> {NewP2, NewP1, NewLog} = Action(S, P2, P1, L), {NewP1, NewP2, NewLog}
    end,
   
    %erlang:display({I2, maps:get(rem_moves, maps:get(state, P2))}),

    {Res1, Res2, ResLog}.

% ======================= MAIN BATTLE LOOP ============================


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{state:=#{hp:=HP1}, id:=I1}, #{state:=#{hp:=HP2}, id:=I2}, Log, FullLog) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if HP1 < 0 -> I2;
                HP2 < 0 -> I1
             end,

    {done, jiffy:encode({[
        {proc, lists:reverse([L || L <- Log, L =/= {[]}])}, {full_log, lists:reverse(FullLog)}, {res,Winner}
    ]} )};


% ------------------ RECALCULATING NEW OFFENDER ----------------------
% after defender consumes last chance of attack, increase the sequence
% number, re-calculate the new offender with their agility, and assign
% the new number of remaining attacks.

% Moreover, since the player's attributes should be recalculated in the
% new round, the attributes should be restored in this stage. Meanwhile,
% both players will be restored to primary hand.

loop(#{seq:=Seq, stage:=Stage}=State,
     #{done:=already, prim_hand:=PrimHand1, orig_attr:=Orig1, state:=State1}=P1,
     #{done:=already, prim_hand:=PrimHand2, orig_attr:=Orig2, state:=State2}=P2,
     L, FL) when (Stage == attacking) or (Stage == preparing)->

    NewP1 = P1#{state:=State1#{rem_moves:=2}, done:=not_yet, curr_hand:=PrimHand1, attr=>Orig1},
    NewP2 = P2#{state:=State2#{rem_moves:=2}, done:=not_yet, curr_hand:=PrimHand2, attr=>Orig2},

    NewMover = toss(NewP1, NewP2),

    erlang:display(' '),
    erlang:display({tossing, Seq+1, NewMover}),

    loop(State#{seq:=Seq+1, stage:=settling, mover:=NewMover}, NewP1, NewP2, L, FL);


% ---------------- SWAPPING OFFENDER AND DEFENDER --------------------

% if not the case above, first we need to determine the guy currently
% moving is offender or defender. If the offender is moving, then we
% simply change to defender without changing anything else. If defender,
% we need to switch to the next phase.

loop(#{stage:=Stage, mover:=Mover}=State, #{done:=already}=P1, #{done:=already}=P2, L, FL) ->

    erlang:display({swapping, Stage, Mover, 1}),
    %erlang:display({maps:get(id, P2), maps:get(rem_moves, maps:get(state, P2))}),

    NewStage = case Stage of
        settling -> casting;
        casting -> attacking
    end,
    
    loop(State#{stage:=NewStage, mover:=swap(Mover, P1, P2)}, P1#{done:=not_yet}, P2#{done:=not_yet}, L, FL);

loop(#{mover:=Mover}=State, #{id:=Mover, done:=already}=P1, #{done:=not_yet}=P2, L, FL) ->
    erlang:display({swapping, Mover, 2}),
    %erlang:display({maps:get(id, P2), maps:get(rem_moves, maps:get(state, P2))}),
    loop(State#{mover:=swap(Mover, P1, P2)}, P1, P2, L, FL);

loop(#{mover:=Mover}=State, #{done:=not_yet}=P1, #{id:=Mover, done:=already}=P2, L, FL) ->
    erlang:display({swapping, Mover, 3}),
    %erlang:display({maps:get(id, P2), maps:get(rem_moves, maps:get(state, P2))}),
    loop(State#{mover:=swap(Mover, P1, P2)}, P1, P2, L, FL);


% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

loop(#{stage:=attacking, mover:=Mover}=S, A, B, L, FL) ->

    {AttackA, AttackB, AttackLog} = trans(fun(State, #{state:=StateO, attr:=CurrAttr}=O, D, Log) ->

        case maps:get(attack_disabled, CurrAttr) of
            0 ->
                erlang:display({attack, Mover, normal}),
                {MovedO, MovedD, MovedLog} = battle_attack:attack(State, O, D, Log),

                DoneMovedO = case maps:get(rem_moves, maps:get(state, MovedO)) of
                    0 -> MovedO#{done:=already};
                    _ -> MovedO
                end,
                
                {DefReactedD, DefReactedO, DefReactedLog} = battle_effect:effect(State, MovedD, DoneMovedO, MovedLog),
                battle_effect:effect(State, DefReactedO, DefReactedD, DefReactedLog);
            _ ->
                erlang:display({attack, Mover, disabled}),

                {O#{done:=already, state:=StateO#{rem_moves:=0}}, D, Log}
        end
    end, S, A, B, L),

    loop(S, AttackA, AttackB, AttackLog,
         [#{seq=>maps:get(seq, S), a=>maps:get(hp, maps:get(state, AttackA)), b=>maps:get(hp, maps:get(state, AttackB))} | FL]);


% ------------------------- LOOP FOR CAST -----------------------------

loop(#{stage:=casting, mover:=Mover}=S, A, B, L, FL) ->

    {CastA, CastB, CastLog} = trans(fun(State, #{casts:=Casts, attr:=CurrAttr}=O, D, Log) ->
        case maps:get(cast_disabled, CurrAttr) of
            0 ->        
                erlang:display({Mover, cast, normal}),
                {MovedO, MovedD, MovedLog} = battle_cast:cast(State, O, D, Log),
                battle_effect:effect(State, MovedO#{done:=already}, MovedD, MovedLog);
            _ ->
                erlang:display({Mover, cast, disabled}),

                ConsumedCasts = case Casts of
                    [] -> [];
                    [_|RemCasts] -> RemCasts
                end,

                {O#{casts:=ConsumedCasts, done:=already}, D, Log}
        end
    end, S, A, B, L),

    loop(S, CastA, CastB, CastLog,
         [#{seq=>maps:get(seq, S), a=>maps:get(hp, maps:get(state, CastA)), b=>maps:get(hp, maps:get(state, CastB))} | FL]);


% ---------------------- LOOP FOR SETTLEMENT -----------------------------

% settlement is the stage that carries out the effects lasted from prior
% rounds. The order follows the order of casting. 

loop(#{stage:=settling, mover:=Mover}=S, A, B, L, FL) ->

    erlang:display({settling, Mover}),

    {SettleA, SettleB, SettleLog} = trans(fun(State, O, D, Log) ->

        battle_effect:effect(State, O#{done:=already}, D, Log)

    end, S, A, B, L),

    loop(S, SettleA, SettleB, SettleLog,
         [#{seq=>maps:get(seq, S), a=>maps:get(hp, maps:get(state, SettleA)), b=>maps:get(hp, maps:get(state, SettleB))} | FL]).



init_new_battle(Data) ->

    {P1, P2} = battle_parse:player_context_from_parsed_JSON(Data),

    {CastedP1, CastedP2, CastedLog} = battle_cast:cast_talented(P1, P2),

    loop(#{seq=>0, stage=>preparing, mover=>maps:get(id, P1)}, CastedP1, CastedP2, CastedLog, []). 
