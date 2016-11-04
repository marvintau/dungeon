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
    SurNames = [{1, "威猛的"}, {2, "霸道的"}, {3, "机智的"}, {4, "勇敢的"}, {5, "无敌的"}, {6, "可爱的"}, {7, "呆萌的"}, {8, "天然的"}, {9,"干练的"}, {10, "飒爽的"}],
    Given = [{1, "总裁"}, {2, "战士"}, {3, "魔法使"}, {4, "隐士"}, {5, "高人"}, {6, "科学家"}, {7, "学霸"}, {8, "天才"}, {9, "学生会长"}, {10, "体育部长"}],

    {_, [{_, ResSurname}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(SurNames) end, SurNamesNames),
    {_, [{_, ResGiven}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(Given) end, Given),

    SurNames++Given.

login(Binary) ->

	{ID, Account} = parse_form(Binary),

	case ets:lookup(players, ID) of
		[] -> ets:insert(players, {{id, ID}, {account, Account}, {display_name, random_name()}, {created_time, created_time}, {last_login_time, last_login_time}, {last_login_location, last_login_location}, {last_login_ip, last_login_ip}}),
			  {received, <<"new_user">>};
		[Res] -> {received, jiffy:encode(Res)}
	end.
