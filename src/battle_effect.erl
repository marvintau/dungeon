
-module(battle_effect).

-author('Yue Marvin Tao').

-export([effect/4]).

% For all single O stands for "Offender" and D for "Defender".

who(off, O, _) -> O;
who(def, _, D) ->D.


% ========================== REFERRING OPERAND ==================================
% expecting {Type, Attribute, PlayerID} or {Type, Attribute, offender/defender}.

ref({Type, Attr, P}) ->
    maps:get(Attr, maps:get(Type, P));
ref({Low, High}) ->
    round(Low + rand:uniform() * (High - Low));
ref(SingleValue) -> SingleValue.

ref_whom({T, A, P}, O, D) -> {T, A, who(P, O, D)};
ref_whom({TAP1, TAP2}, O, D) -> {ref_whom(TAP1, O, D), ref_whom(TAP2, O, D)}.

ref_whom_get({T, A, P}, O, D) -> ref(ref_whom({T, A, P}, O, D));
ref_whom_get({TAP1, TAP2}, O, D) -> {ref_whom_get(TAP1, O, D), ref_whom_get(TAP2, O, D)}.

% =========================== TRANSFER FUNCTIONS ===============================
% Apply transfer operations over specific attributes of player context. The type
% could be varying state (hp, remaining moves) or attribute (hit, dodge, block,
% etc.) that resets for every round. The supported operations include get, set,
% add, add & multiply original value, or the value referring to other attributes.

% trans cares if the damage will be absorbed by the armor of defender.
%
% expecting {Opcode, Value, React} where opcode of set/add/add_mul/add_inc_mul,
% and Value of number, interval or {type, attribute, off/def} triple.

trans({set, New, _}, {Type, Attr, P}) ->
    #{Type:=#{Attr:=Old}=TypeInstance} = P,
    ReferredNew = ref(New),
    case is_number(ReferredNew) of
        true -> P#{Type:=TypeInstance#{Attr:=round(ref(New)), diff:=round(ref(New) - Old)}};
        _    -> P#{Type:=TypeInstance#{Attr:=ref(New)}}
    end;

trans({add, Inc, Cond}, {_, _, P}=ToWhom) when (Cond == absorbable) or (Cond== both)->
    ArmorRatio = 1 - ref({attr, armor, P}) / 10000,
    trans({set, ref(ToWhom) + ref(Inc) * ArmorRatio, none}, ToWhom);

trans({add, Inc, none}, ToWhom) ->
    trans({set, ref(ToWhom) + ref(Inc), none}, ToWhom);

trans({add_mul, Mul, Absorbing}, ToWhom) ->
    trans({add, ref(ToWhom) * ref(Mul), Absorbing}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, Absorbing}, ToWhom) ->
    trans({add, ref(Inc) * ref(Mul), Absorbing}, ToWhom).


% ========================== APPLY TRANS FUNCTION ===============================
% apply_trans function will introduce the actual player context, and calculates
% the result after applying the trans operations. Notably, apply_trans handles all
% three conditions of resistable, absorbable, and both. For resistable and both,
% if the random generated number is less than the resist attribute of defender, then
% the original offender and defender will be returned. Otherwise, the both will be
% handled as absorbable.

apply_trans({{Opcode, Oper, AddCond}, {_T, _A, P}=ToWhom}, O, D) ->

    RefOperand = case Oper of
        {Tr, Ar, Pr} -> ref_whom_get({Tr, Ar, Pr}, O, D);
        {{_, _, _} = TAP1, {_, _, _}=TAP2} -> ref_whom_get({TAP1, TAP2}, O, D);
        Value -> ref(Value)
    end,

    RefWhom = ref_whom(ToWhom, O, D),

    IsResisted = case AddCond of
        resistable -> rand:uniform() > maps:get(resist, maps:get(attr, D));
        both -> rand:uniform() > maps:get(resist, maps:get(attr, D));
        _ -> false
    end,

    case {IsResisted, P} of
        {true, _} -> {resisted, ToWhom, O, D};
        {_, off} -> {effected, ToWhom, trans({Opcode, RefOperand, AddCond}, RefWhom), D};
        {_, def} -> {effected, ToWhom, O, trans({Opcode, RefOperand, AddCond}, RefWhom)}
    end.

log(#{seq:=Seq, stage:=Stage, mover:=Mover}, EffName, Outcome, {_, {T, Attr, P}}, O, D) when (Outcome==resisted) or (Attr==hp)->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, who(P, O, D))},
        { hand, none}, { action, EffName},
        { outcome, Outcome }, { damage, maps:get(diff, maps:get(T, who(P, O, D))) },
        { offender_hp, maps:get(hp, maps:get(state, O)) },
        { defender_hp, maps:get(hp, maps:get(state, D)) }
    ]};

log(_, _, _, _, _, _) -> {[]}.

% ======================== APPLY ALL TRANSFERS IN A LIST ========================
% For each trans operation, apply_trans_with_log combines the player context with
% log. Since the transfers are written in a list, the apply_trans_all.g_nested will
% apply all the transfers sequentially over the player context, and returns log.

apply_trans_logged(EffName, Trans, S, O, D) ->
   

    {EffectStatus, _Destination, TransedO, TransedD} = apply_trans(Trans, O, D),

    {TransedO, TransedD, log(S, EffName, EffectStatus, Trans, TransedO, TransedD)}.

apply_trans_all(EffName, TransList, S, O, D) ->
    apply_trans_all(EffName, TransList, S, O, D, []).
apply_trans_all(EffName, [Trans | RemTrans], S, O, D, Logs) ->

    erlang:display({EffName, Trans}),

    {AppliedO, AppliedD, L} = apply_trans_logged(EffName, Trans, S, O, D),
    apply_trans_all(EffName, RemTrans, S, AppliedO, AppliedD, [L | Logs]);
apply_trans_all(_EffName, [], _S, O, D, Logs) ->
    {O, D, Logs}.

% ======================== CHECK SINGLE CONDITION ===============================
% besides the round control, the condition checking stage also goes through other
% attributes checking, such as the outcome of last attack.
cond_single({Val, '==', TAP}, O, D) -> 
    Val == ref_whom_get(TAP, O, D);
cond_single({Val, '>', TAP}, O, D) -> Val > ref_whom_get(TAP, O, D);
cond_single({Val, '<', TAP}, O, D) -> Val < ref_whom_get(TAP, O, D).

cond_list(CondList, O, D) -> cond_list(true, CondList, O, D).
cond_list(TrueValue, [Cond | RemConds], O, D) ->
    cond_list(TrueValue and cond_single(Cond, O, D), RemConds, O, D);
cond_list(TrueValue, [], _, _) -> TrueValue.


% ====================== SEQUENTIAL CONDITION CHECK =============================
% checks whether the battle goes to specific round and stage.
seq_cond({StartingSeq, TerminalSeq, Phase}, #{seq:=CurrSeq, stage:=CurrStage}) ->


    % If StartingSeq - CurrSeq is zero, that means we are in the current round
    % that the effect is casted, which means the effect should be applied right
    % after the casting. Otherwise, the effect is in the following rounds after
    % casted, which should be applied in settling stage.
    CalculatedPhase = case {Phase, StartingSeq - CurrSeq} of
        {casting, 0} -> casting;
        {casting, _} -> settling;
        {_, _} -> Phase
    end,

    (CurrSeq >= StartingSeq) and (CurrSeq < TerminalSeq) and (CalculatedPhase == CurrStage).

% ============================ CONDITION CHECK  =================================
% checking both of sequential and additional conditions.

cond_check({SeqCond, CondList}, S, O, D) ->
    seq_cond(SeqCond, S) and cond_list(CondList, O, D).

% =========================== APPLYING EFFECTS ==================================
% wrapper function to apply the effects over the player context, and mark whether
% the context is modified. If modified, the function returns {affected, P1, P2},
% otherwise {not_affected, P1, P2}

apply_effect(Effect, State, {O, D}) ->

    {Name, Conds, Specs} = Effect,

    case cond_check(Conds, State, O, D) of
        
        true ->
            apply_trans_all(Name, Specs, State, O, D);
        
        _    ->
            {Seq, _} = Conds,
            {O, D, []}
    end.


% ============================ EFFECT ===========================================
% for each stage, effect processes the whole effect table, get the final player
% context and log.

effect(S, #{effects:=Effects}=O, D, Log) ->

    effect(S, O, D, Log, Effects).

effect(_S, #{hp:=H1}=O, #{hp:=H2}=D, Log, _) when (H1 =< 0) or (H2 =< 0) ->
    {O, D, Log};
    
effect(_S, O, D, Log, []) ->
    {O, D, Log};

effect(S, O, D, Log, [EffectSpec| Remaining]) ->
    {EffectedOffender, EffectedDefender, NewLog} = apply_effect(EffectSpec, S, {O, D}),

    effect(S, EffectedOffender, EffectedDefender, lists:append(NewLog, Log), Remaining).

