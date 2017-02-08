-module(battle).

-author('Yue Marvin Tao').

-export([new/1]).

rand() ->
    element(3, erlang:timestamp())/1000000.

% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

% Any cast that might change the normal way of determine the order of
% attack will be put here.

toss(#{casts:=[rune_of_the_void|_], id:=A}, _) ->
    A;
toss(_, #{casts:=[rune_of_the_void|_], id:=B}) ->
    B;

toss(#{id:=A, attr:=#{agility:=AgiA}},
     #{id:=B, attr:=#{agility:=AgiB}}) ->
    case rand() * (AgiA + AgiB) > AgiA of
        true -> B;
        _    -> A
    end.

% ----------- HELPER FUNCTION FOR SWAPPING OFFENDER/DEFENDER -----------

swap(Mover, #{id:=Mover}, #{id:=B}) -> B;
swap(_, #{id:=A}, _) -> A.

% ======================= MAIN BATTLE LOOP ============================


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{state:=#{hp:=HP1}, id:=I1}, #{state:=#{hp:=HP2}, id:=I2}, Log, FullLog) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if HP1 < 0 -> I2;
                HP2 < 0 -> I1
             end,

    {done, 
        {records, lists:reverse([L || L <- Log, L =/= {[]}])}, {full_log, lists:reverse(FullLog)}, {winner,Winner}
    };


% ------------------ RECALCULATING NEW OFFENDER ----------------------
% after defender consumes last chance of attack, increase the sequence
% number, re-calculate the new offender with their agility, and assign
% the new number of remaining attacks.

% Moreover, since the player's attributes should be recalculated in the
% new round, the attributes should be restored in this stage. Meanwhile,
% both players will be restored to primary hand.

loop(#{seq:=Seq, stage:=attacking}=State,
     #{done:=already, prim_hand:=PrimHand1, orig_attr:=Orig1, state:=#{position:=PosP1}=State1}=P1,
     #{done:=already, prim_hand:=PrimHand2, orig_attr:=Orig2, state:=#{position:=PosP2}=State2}=P2,
     L, FL) ->

    NewP1 = P1#{state:=State1#{rem_moves:=2, position:=PosP1}, done:=not_yet, curr_hand:=PrimHand1, attr:=Orig1},
    NewP2 = P2#{state:=State2#{rem_moves:=2, position:=PosP2}, done:=not_yet, curr_hand:=PrimHand2, attr:=Orig2},

    NewMover = toss(NewP1, NewP2),

    EmptyLog = case L of
        [{[{_, LastLogSeq}|_]} |_] -> case Seq - LastLogSeq > 1 of
                true -> {[
                        { seq, LastLogSeq+1 }, {stage, rest}, { offender, rest },
                        { action, rest},
                        { effects, [] }, { damage, 0 },
                        { offenderHP, 0 },
                        { defenderHP, 0 },
                        { offenderPos, PosP1 }, {defenderPos, PosP2}
                    ]};
                _ -> {[]}
            end;
        _ -> {[]}
    end,

%    erlang:display(' '),
   erlang:display({new_round, Seq+1, NewMover}),

    loop(State#{seq:=Seq+1, stage:=settling, mover:=NewMover}, NewP1, NewP2, [EmptyLog | L], FL);


% ---------------- SWAPPING OFFENDER AND DEFENDER --------------------

% if not the case above, first we need to determine the guy currently
% moving is offender or defender. If the offender is moving, then we
% simply change to defender without changing anything else. If defender,
% we need to switch to the next phase.

loop(#{stage:=settling, mover:=Mover, is_opening:=IsOpening}=State, #{done:=already}=P1, #{done:=already}=P2, L, FL) ->

    NextStage = case IsOpening of
        true -> opening;
        _    -> casting
    end,

    loop(State#{stage:=NextStage, mover:=swap(Mover, P1, P2)}, P1#{done:=not_yet}, P2#{done:=not_yet}, L, FL);


loop(#{stage:=opening, mover:=Mover}=State, #{done:=already}=P1, #{done:=already}=P2, L, FL) ->
    loop(State#{is_opening:=false, stage:=attacking, mover:=swap(Mover, P1, P2)}, P1#{done:=already}, P2#{done:=already}, L, FL);

loop(#{stage:=casting, mover:=Mover}=State, #{done:=already}=P1, #{done:=already}=P2, L, FL) ->
    loop(State#{stage:=attacking, mover:=swap(Mover, P1, P2)}, P1#{done:=not_yet}, P2#{done:=not_yet}, L, FL);

loop(#{mover:=Mover}=State, #{id:=Mover, done:=already}=P1, #{done:=not_yet}=P2, L, FL) ->
    loop(State#{mover:=swap(Mover, P1, P2)}, P1, P2, L, FL);

loop(#{mover:=Mover}=State, #{done:=not_yet}=P1, #{id:=Mover, done:=already}=P2, L, FL) ->
    loop(State#{mover:=swap(Mover, P1, P2)}, P1, P2, L, FL);


% ------------------------ LOOP FOR ATTACK  ---------------------------
% In order to guarantee that there is no status dependency, all status
% modification regarding attributes will be restored except HP, number
% of remaining attacks current gamer in move.

loop(#{stage:=attacking, mover:=Mover, seq:=Seq}=S, #{id:=IDA, attr:=AttrA}=A, #{id:=IDB, attr:=AttrB}=B, L, FL) ->

    % erlang:display({Seq, maps:get(done, A), maps:get(done, B)}),

    {#{state:=#{hp:=HpA}}=AttackA, #{state:=#{hp:=HpB}}=AttackB, AttackLog} = case Mover of
        IDA -> attack:apply(S, A, B, L);
        IDB -> {NewB, NewA, NewLog} = attack:apply(S, B, A, L), {NewA, NewB, NewLog}
    end,

    loop(S, AttackA#{attr:=AttrA#{outcome:=none}}, AttackB#{attr:=AttrB#{outcome:=none}}, AttackLog, [#{seq=>Seq, a=>HpA, b=>HpB} | FL]);


% ------------------------- LOOP FOR CAST -----------------------------

loop(#{stage:=opening, mover:=Mover, seq:=Seq}=S, #{id:=IDA}=A, #{id:=IDB}=B, L, FL) ->

    {#{state:=#{hp:=HpA}}=OpeningA, #{state:=#{hp:=HpB}}=OpeningB, OpeningLog} = case Mover of
        IDA -> cast:apply_opening(S, A, B, L);
        IDB -> {NewB, NewA, NewLog} = cast:apply_opening(S, B, A, L), {NewA, NewB, NewLog}
    end,

    loop(S, OpeningA, OpeningB, OpeningLog, [#{seq=>Seq, a=>HpA, b=>HpB} | FL]);


loop(#{stage:=casting, mover:=Mover, seq:=Seq}=S, #{id:=IDA}=A, #{id:=IDB}=B, L, FL) ->

    {#{state:=#{hp:=HpA}}=CastA, #{state:=#{hp:=HpB}}=CastB, CastLog} = case Mover of
        IDA -> cast:apply(S, A, B, L);
        IDB -> {NewB, NewA, NewLog} = cast:apply(S, B, A, L), {NewA, NewB, NewLog}
    end,

    loop(S, CastA, CastB, CastLog, [#{seq=>Seq, a=>HpA, b=>HpB} | FL]);


% ---------------------- LOOP FOR SETTLEMENT -----------------------------

% settlement is the stage that carries out the effects lasted from prior
% rounds. The order follows the order of casting.

loop(#{stage:=settling, mover:=Mover, seq:=Seq}=S, #{id:=IDA}=A, #{id:=IDB}=B, L, FL) ->

    {#{state:=#{hp:=HpA}}=SettleA, #{state:=#{hp:=HpB}}=SettleB, SettleLog} = case Mover of
        IDA -> effect:apply(S, A#{done:=already}, B, L);
        IDB -> {NewB, NewA, NewLog} = effect:apply(S, B#{done:=already}, A, L), {NewA, NewB, NewLog}
    end,

    loop(S, SettleA, SettleB, SettleLog, [#{seq=>Seq, a=>HpA, b=>HpB} | FL]).



new({#{id:=Id, orig_attr:=OrigP1}=P1, #{orig_attr:=OrigP2}=P2}) ->

    erlang:display(battle_begins),

    State = #{
        seq=>-1,
        stage=>attacking,
        mover=>Id,
        is_opening=>true
    },

    loop(State, P1#{attr=>OrigP1, done:=already}, P2#{attr=>OrigP2, done:=already}, [], []).

