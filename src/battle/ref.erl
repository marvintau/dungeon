-module(ref).

-compile({no_auto_import,[get/1]}).
-export([get/1, get/3, set/2, set/4, val/1, val/3, who_this/3, who_that/3]).

who_this(off, O, _D) -> O;
who_this(def, _O, D) -> D.

who_that(off, _O, D) -> D;
who_that(def, O, _D) -> O.



get({AttrType, Attr, P}) ->
	#{AttrType:=#{Attr:=Val}} = P, Val.

get({AttrType, Attr, P}, O, D) ->
	get({AttrType, Attr, who_this(P, O, D)}).



set({AttrType, Attr, P}=Ref, Val) when is_number(Val) ->
	#{AttrType:=AttrSet} = P,
	P#{AttrType:=AttrSet#{ Attr:=round(Val), diff:=round(Val-get(Ref)) }};

set({AttrType, Attr, P}, Val) ->
	#{AttrType:=AttrSet} = P,
	P#{AttrType:=AttrSet#{ Attr:=round(Val) }}.

set({AttrType, Attr, P}, O, D, Val) ->
	set({AttrType, Attr, who_this(P, O, D)}, Val).



val({_, _, _}=Ref) ->
	get(Ref);

val({Low, High}) ->
    round(Low + rand:uniform() * (High - Low));

val(SingleValue) -> SingleValue.

val({AttrType, Attr, P}, O, D) ->
	val({AttrType, Attr, who_this(P, O, D)});
val({Low, High}, _O, _D) ->
	round(Low + rand:uniform() * (High - Low));
val(SingleVal, _O, _D) ->
	val(SingleVal).
