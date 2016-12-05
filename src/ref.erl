-module(ref).

-compile({no_auto_import,[get/1]}).
-export([get/1, val/1, set/2]).

% Ref: a frequently used data structure that refers to an attribute
% of specific player.

get({AttrType, Attr, P}) ->
	#{AttrType:=#{Attr:=Val}} = P, Val.


set({AttrType, Attr, P}=Ref, Val) when is_number(Val) ->
	#{AttrType:=AttrSet} = P,
	P#{AttrType:=AttrSet#{ Attr:=round(Val), diff:=round(Val-get(Ref)) }};

set({AttrType, Attr, P}, Val) ->
	#{AttrType:=AttrSet} = P,
	P#{AttrType:=AttrSet#{ Attr:=round(Val) }}.


val({_, _, _}=Ref) ->
	get(Ref);

val({Low, High}) ->
    round(Low + rand:uniform() * (High - Low));

val(SingleValue) -> SingleValue.

