-module(effect).

-author('Yue Marvin Tao').

-export([apply_effects/1]).
-export([effect/2]).
-export([cond_always/0, cond_last_for/1]).

% Apply effect iterates over the effect table and apply the available
% effects, and returns the final {A, D, B, L} after all effects are
% applied.

apply_single_effect({A, D, B}, {EffectName, EffectFunc}) ->
    EffectFunc({A, D, B#{effect_name:=EffectName}}).

apply_effects({A, D, B}, []) ->
    {A, D, B};
apply_effects({A, D, B}, [Effect | Rem]) ->
    apply_effects(apply_single_effect({A, D, B}, Effect), Rem).

apply_effects({A, D, #{effect_action_list:=EffectList}=B}) ->
    apply_effects({A, D, B}, EffectList).

% Effect returns an anonymous function that evaluated each time.
% effect doesn't care about the conditions of applying effects,
% because everything has been specified in Condition, which is
% supposed to be specified in another function

% An effect should follow the structure: {EffectName, EffectAction} 
% where EffcetName is fetched from a list that predefined by user,
% and will be logged into the procedure, and returned to the client.

% The Applying of effect will be splitted into two phases, the first
% phase will be "mounting". For each turn, the main battle loop will
% apply the ACTION that defined by user. If the action meets the re-
% quirement (like not invalidated by opponent's action), a STATUS
% will be mounted on the current active player, or the opponent.

% The second phase will be applying the effect, which iterates over
% the list containing all mounted STATUS, and find the corresponding
% condition checker and action that finally modifies player's context.

effect(Condition, EffectAction) ->
    fun({A, D, B}) -> case Condition({A, D, B}) of
        true ->
            EffectAction({A, D, B});
        _ ->
            {A, D, B}
    end end.

% ===== HELPER FUNCTIONS FOR DESCRIBING CONDITIONS ==============

cond_always() ->
    fun(_) -> true end.

cond_last_for(Turns) ->
    fun({_, _, #{seq_no:=SeqNo}, _}) -> SeqNo < Turns end.
