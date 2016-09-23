-module(cast).

-author('Yue Marvin Tao').

-export([shield_slam/1, pierce_strike/1, ice_storm/1]).
-export([cast/3]).

% A status entry in a list should conform to the format of:
%

shield_slam(B) ->
    shield_slam(B, fun(_, _, _) -> true end).

shield_slam(#{status:=Status}=Battle, CondFunc) ->
    Battle#{status:=[
        {shield_slam_buff, pre, CondFunc, fun({#{block:=Block} = A, D, B}) ->
            {A#{block := Block + 15 }, D, B} end
        },
        
        {shield_slam_damage, post, CondFunc, fun({#{hp:=HP} = A, D, #{outcome:=Outcome} = B}) ->
            ShieldDamage = case Outcome of
                block -> 125;
                _     -> 0
            end,

            {A#{hp:=HP - ShieldDamage}, D, B} end    
        }
        
    | Status]}.

pierce_strike(B) ->
    pierce_strike(B, fun(_, _, _) -> true end).

pierce_strike(#{status:=Status}=Battle, CondFunc) ->
    Battle#{status:=[

        {pierce_strike_buff, pre, CondFunc, fun({#{critic:=Critic}=A, D, B}) ->
            {A#{critic := Critic + 10}, D, B} end
        },

        {pierce_strike_damage, post, CondFunc, fun({A, #{hp:=HP} = D, #{outcome:=Outcome, damage:=Damage} = B}) ->
            PierceDamage = case Outcome of
                critic -> Damage * 1.5;
                _ -> Damage
            end,

            {A, D#{hp:= HP - PierceDamage}, B} end
        }

    | Status]}.

ice_storm(B) ->
    ice_storm(B, fun(_, _, _) -> true end).

ice_storm(#{status:=Status}=Battle, CondFunc) ->
    Battle#{status:=[
    
        {ice_storm, pre, CondFunc, fun({A, D, B}) ->
            {A#{attack:=0, critic := 100}, D#{block := 0, dodge := 0, resist :=0, curr_hand := {frozen, {no_damage, bare}, {0, 0}}}, B} end
        }

    | Status]}.

mount_cast(#{id:=I1, curr_cast:=Cast}=P1, P2, #{offenser:=Off}=B) when I1 == Off ->
    apply(cast, Cast, [B]);
mount_cast(P1, #{curr_cast:=Cast}=P2, B) ->
    apply(cast, Cast, [B]).

cast(#{id:=I1}=P1, P2, #{offenser:=Off}=B) when I1 == Off ->
    perform_cast(P1, P2, B);
cast(P1, P2, B) ->
    {NewP2, NewP1, NewB} = perform_cast(P1, P2, B),
    {NewP1, NewP2, NewB}.

