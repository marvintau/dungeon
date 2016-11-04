-module(player_database).

-author('Yue Marvin Tao').

-export([init_database/0, parse_form/1, login/1, random_name/0]).

init_database() ->
    ets:new(players, [set, public, named_table]).


parse_form(Binary) ->

    case [binary:split(Field, <<"=">>) || Field <- binary:split(Binary, <<"&">>)] of
    	[[_, ID], [_, Account]] -> {ID, Account};
    	NotExpected -> {malformed, NotExpected}
    end.


random_name() ->
    Names = [{1, "威猛的总裁"}, {2, "霸道的战士"}, {3, "机智的魔法使"}, {4, "勇敢的隐士"}, {5, "无敌的高人"}, {6, "可爱的科学家"}, {7, "呆萌的学霸"}, {8, "天然的天才"}, {9,"干练的学生会长"}, {10, "飒爽的体育部长"}],
    erlang:display(5 > rand:uniform() * length(Names)),
    {_, [{_, Res}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(Names) end, Names),
    Res.

login(Binary) ->

	{ID, Account} = parse_form(Binary),

	case ets:lookup(players, ID) of
		[] -> ets:insert(players, {ID, Account}),
			  {received, <<"new_user">>};
		[Res] -> {received, jiffy:encode(Res)}
	end.
