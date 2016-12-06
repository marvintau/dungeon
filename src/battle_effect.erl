
-module(battle_effect).

-author('Yue Marvin Tao').

-export([effect/4]).

log(EffName, Mover, #{seq:=Seq, stage:=Stage}, #{state:=#{hp:=HpO}}, #{state:=#{hp:=HpD}}, EffectNote, Logs) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, who},
        { hand, none}, { action, EffName}, {outcome_note, EffectNote},
        { outcome, Logs }, { damage, 0 },
        { offender_hp, HpO },
        { defender_hp, HpD }
    ]}.


% ======================== APPLY ALL TRANSFERS IN A LIST ========================
% For each trans operation, apply_trans_with_log combines the player context with
% log. Since the transfers are written in a list, the apply_trans_all.g_nested will
% apply all the transfers sequentially over the player context, and returns log.
   


apply_trans_all(EffName, Last, Mover, TransList, EffectNote, S, O, D) ->
    apply_trans_all(EffName, Last, Mover, TransList, EffectNote, S, O, D, []).

apply_trans_all(EffName, Last, Mover, [Trans | RemTrans], EffectNote, S, #{id:=OID}=O, #{id:=DID}=D, Logs) ->

    {{Status, Dest, Diff}, TransedO, TransedD} = trans:trans(Trans, O, D),

    {_, A, P} = Dest,

    Log = {[{status, Status}, {last_round, Last}, {dest, {[{attr, A}, {person, ref:who_this(P, OID, DID)}]}}, {diff, Diff}]},

    apply_trans_all(EffName, Last, Mover, RemTrans, EffectNote, S, TransedO, TransedD, [ Log | Logs]);

apply_trans_all(EffName, _Last, Mover, [], EffectNote, S, O, D, Logs) ->
    {O, D, [log(EffName, Mover, S, O, D, EffectNote, Logs)]}.


% ============================ CONDITION CHECK  =================================
% checking both of sequential and additional conditions.


% =========================== APPLYING EFFECTS ==================================
% wrapper function to apply the effects over the player context, and mark whether
% the context is modified. If modified, the function returns {affected, P1, P2},
% otherwise {not_affected, P1, P2}

apply_effect(Effect, State, {O, D}) ->

    {Name, Mover, {{_Start, Last, _Phase}, _} = Conds, Specs, EffectNote} = Effect,

    case conds:check(Conds, State, O, D) of
        
        true ->
            apply_trans_all(Name, Last, Mover, Specs, EffectNote, State, O, D);
        
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

