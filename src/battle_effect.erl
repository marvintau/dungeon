
-module(battle_effect).

-author('Yue Marvin Tao').

-export([effect/4]).


log(#{seq:=Seq, stage:=Stage}, EffName, Mover, resisted, {_, {_, _, P}}, #{state:=#{hp:=HpO}}=O, #{state:=#{hp:=HpD}}=D) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, who},
        { hand, none}, { action, EffName},
        { outcome, resisted }, { damage, 0 },
        { offender_hp, HpO },
        { defender_hp, HpD }
    ]};

log(#{seq:=Seq, stage:=Stage}, EffName, Mover, _, {_, {_, hp, P}}, #{state:=#{hp:=HpO}}=O, #{state:=#{hp:=HpD}}=D) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, who},
        { hand, none}, { action, EffName},
        { outcome, effected }, { damage, HpD },
        { offender_hp, HpO },
        { defender_hp, HpD }
    ]};

log(_, _, _, _, _, _, _) -> {[]}.

% ======================== APPLY ALL TRANSFERS IN A LIST ========================
% For each trans operation, apply_trans_with_log combines the player context with
% log. Since the transfers are written in a list, the apply_trans_all.g_nested will
% apply all the transfers sequentially over the player context, and returns log.

apply_trans_logged(EffName, Mover, Trans, S, O, D) ->
   
    {EffectStatus, _Destination, TransedO, TransedD} = trans:trans(Trans, O, D),

    {TransedO, TransedD, log(S, EffName, Mover, EffectStatus, Trans, TransedO, TransedD)}.

apply_trans_all(EffName, Mover, TransList, S, O, D) ->
    apply_trans_all(EffName, Mover, TransList, S, O, D, []).
apply_trans_all(EffName, Mover, [Trans | RemTrans], S, O, D, Logs) ->

    {AppliedO, AppliedD, L} = apply_trans_logged(EffName, Mover, Trans, S, O, D),
    apply_trans_all(EffName, Mover, RemTrans, S, AppliedO, AppliedD, [L | Logs]);
apply_trans_all(_EffName, _Mover, [], _S, O, D, Logs) ->
    {O, D, Logs}.

% ======================== CHECK SINGLE CONDITION ===============================
% besides the round control, the condition checking stage also goes through other
% attributes checking, such as the outcome of last attack.
cond_single({Val, '==', TAP}, O, D) -> 
    Val == ref:val(TAP, O, D);
cond_single({Val, '>', TAP}, O, D) ->
    Val > ref:val(TAP, O, D);
cond_single({Val, '<', TAP}, O, D) ->
    Val < ref:val(TAP, O, D).

cond_list(CondList, O, D) -> cond_list(CondList, O, D, true).
cond_list([Cond | RemConds], O, D, TrueValue) ->
    cond_list(RemConds, O, D, TrueValue and cond_single(Cond, O, D));
cond_list([], _, _, TrueValue) -> TrueValue.


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

    {Name, Mover, Conds, Specs} = Effect,

    case cond_check(Conds, State, O, D) of
        
        true ->
            apply_trans_all(Name, Mover, Specs, State, O, D);
        
        _    ->
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

