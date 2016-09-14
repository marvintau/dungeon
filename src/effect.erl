-module(effect).

-author('Yue Marvin Tao').

-include("battle.hrl").

-export([ ]).

is_after_the_turn(Number) ->
    fun({_, _, B}) -> B#battle.seq_no > Number end.

%apply_effect()

% Effect returns an anonymous function that evaluated each time.
% effect doesn't care about the conditions of applying effects,
% because everything has been specified in Condition, which is
% supposed to be specified in another function
effect(Condition, EffectAction) ->
    fun({A, D, B}) -> case Condition({A, D, B}) of
        true ->
            EffectAction({A, D, B});
        _ ->
            {A, D, B}
    end end.


