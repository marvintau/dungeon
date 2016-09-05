-module(roulette).

-author('Yue Marvin Tao').
-export([turn_roulette/1, prepare_roulette/2]).

-include ("battle.hrl").

% ================= REVERSED ACCUMULATED LIST =======================

% A helper function that yields a list with its elements successively
% accumulated and reversed.

accum_reversed(List) ->
    accum_reversed(List, []).

accum_reversed([A|Rem], []) ->
    accum_reversed(Rem, [A]);

accum_reversed([A|Rem], Res) ->
    accum_reversed(Rem, [A + hd(Res) | Res]);

accum_reversed([], Res) ->
    Res.

% ================= TURN THE ROULETTE TABLE ==========================

% Yields an index of the slot number, which conform to the segmented
% distribution function.

turn_roulette(RouletteTable) ->
    
    AccumulatedRoulette = accum_reversed(RouletteTable),

    Rand = random:uniform() * hd(AccumulatedRoulette),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, AccumulatedRoulette))),
    lists:nth(ResultIndex, [block, resist, dodge, critical, attack]).

prepare_roulette(Attacker, Defenser) ->
    
    PlainHit = 8,

    {Dodge, Resistance, Block} = case Attacker#context.char#char.type of
        mage ->
            {0, Attacker#context.resistance, 0};
        _ ->
            {Defenser#context.dodge, 0, Defenser#context.block}
    end,
            
    [Attacker#context.hit + PlainHit, Attacker#context.critical, Dodge, Resistance, Block].


