-module(battle_cast).

-author('Yue Marvin Tao').

-export([shield_slam/1, pierce_strike/1, ice_storm/1, assault/1]).
-export([cast/3]).

% A effects entry in a list should conform to the format of:

assault(#{seq_no:=StartSeq}=B) ->
    assault(B, fun(_, _, #{seq_no:=Seq}) -> Seq =< StartSeq + 1 end).

assault(#{effects:=Effects, offender:=Off}=Battle, CondFunc) ->
    Battle#{effects:=[
        {assualt_damage, pre, Off, CondFunc, fun(A, #{hp:=Hp}=D, B) ->
            {A, D#{hp:=Hp - 100}, B} end
        }
    | Effects]}.

shield_slam(B) ->
    shield_slam(B, fun(_, _, _) -> true end).

shield_slam(#{effects:=Effects, offender:=Off}=Battle, CondFunc) ->
    Battle#{effects:=[
        {shield_slam_buff, pre, Off, CondFunc, fun({#{block:=Block} = A, D, B}) ->
            {A#{block := Block + 15 }, D, B} end
        },
        
        {shield_slam_damage, post, Off, CondFunc, fun({#{hp:=HP} = A, D, #{outcome:=Outcome} = B}) ->
            ShieldDamage = case Outcome of
                block -> 125;
                _     -> 0
            end,

            {A#{hp:=HP - ShieldDamage}, D, B} end    
        }
        
    | Effects]}.

pierce_strike(B) ->
    pierce_strike(B, fun(_, _, _) -> true end).

pierce_strike(#{effects:=Effects, offender:=Off}=Battle, CondFunc) ->
    Battle#{effects:=[

        {pierce_strike_buff, pre, Off, CondFunc, fun({#{critic:=Critic}=A, D, B}) ->
            {A#{critic := Critic + 10}, D, B} end
        },

        {pierce_strike_damage, post, Off, CondFunc, fun({A, #{hp:=HP} = D, #{outcome:=Outcome, damage:=Damage} = B}) ->
            PierceDamage = case Outcome of
                critic -> Damage * 1.5;
                _ -> Damage
            end,

            {A, D#{hp:= HP - PierceDamage}, B} end
        }

    | Effects]}.

ice_storm(B) ->
    ice_storm(B, fun(_, _, _) -> true end).

ice_storm(#{effects:=Effects, offender:=Off}=Battle, CondFunc) ->
    Battle#{effects:=[
    
        {ice_storm, pre, Off, CondFunc, fun({A, D, B}) ->
            {A#{attack:=0, critic := 100}, D#{block := 0, dodge := 0, resist :=0, curr_hand := {frozen, {no_damage, bare}, {0, 0}}}, B} end
        }

    | Effects]}.

% Insert the cast of current offender into the list. It's only aware
% of who is the current offender.
cast(#{id:=I1, curr_cast:=Cast}, _, #{offender:=Off}=B) when I1 == Off ->
    apply(?MODULE, Cast, [B]);
cast(_, #{curr_cast:=Cast}, B) ->
    apply(?MODULE, Cast, [B]).


