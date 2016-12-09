-module(conds).

-export([seq/2, check/4]).

rand() -> element(3, erlang:timestamp())/1000000.

seq({{Start, {Last1, Last2}, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, rand() * (Last2 - Last1) + Last1, Phase}, Others};

seq({{Start, null, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, 9999, Phase}, Others};

seq({{Start, Last, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, CurrSeq + Start + Last, Phase}, Others}.


% The reason that we cannot replace the off/def with actual ID is, that
% we don't know who is off/def when battle starts.
comp({Val, '==', TAP}, O, D) -> 
    Val == ref:val(TAP, O, D);

comp({Val, '>', TAP}, O, D) ->
    Val > ref:val(TAP, O, D);

comp({Val, '<', TAP}, O, D) ->
    Val < ref:val(TAP, O, D).



comps(CondList, O, D) ->
	comps(CondList, O, D, true).

comps([Cond | RemConds], O, D, TrueValue) ->
    comps(RemConds, O, D, TrueValue and comp(Cond, O, D));

comps([], _, _, TrueValue) ->
	TrueValue.


% ====================== SEQUENTIAL CONDITION CHECK =============================
% checks whether the battle goes to specific round and stage.
seq_check({StartingSeq, TerminalSeq, Phase}, #{seq:=CurrSeq, stage:=CurrStage}) ->

    CalculatedPhase = case {Phase, StartingSeq - CurrSeq} of
        {casting, 0} -> casting;
        {casting, _} -> settling;
        {_, _} -> Phase
    end,

    (CurrSeq >= StartingSeq) and (CurrSeq < TerminalSeq) and (CalculatedPhase == CurrStage).

check({SeqCond, CondList}, S, O, D) ->
    seq_check(SeqCond, S) and comps(CondList, O, D).
