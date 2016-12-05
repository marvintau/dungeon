-module(ref).

-export([ref/1]).

% Ref: a frequently used data structure that refers to an attribute
% of specific player.

ref({ref, hp, #{state:=#{hp:=Hp}}}) -> Hp;
ref({ref, rem_moves, #{state:=#{rem_moves:=RemMoves}}}) -> RemMoves;

ref({ref, Attr, P}) -> 
    #{attr:=#{Attr:=Value}} = P,
    Value;

ref({Low, High}) ->
    round(Low + rand:uniform() * (High - Low));
ref(SingleValue) -> SingleValue.

