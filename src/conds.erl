-module(conds).

-export([seq/2]).

rand() -> element(3, erlang:timestamp())/1000000.

seq({{Start, {Last1, Last2}, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, rand() * (Last2 - Last1) + Last1, Phase}, Others};

seq({{Start, null, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, 9999, Phase}, Others};

seq({{Start, Last, Phase}, Others}, CurrSeq) ->
    {{CurrSeq + Start, CurrSeq + Start + Last, Phase}, Others}.

