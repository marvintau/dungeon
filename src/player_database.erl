-module(player_database).

-author('Yue Marvin Tao').

-export([init_database/0, parse_form/1, login/1]).

init_database() ->
    ets:new(players, [set, public, named_table]).


parse_form(Binary) ->

    case [binary:split(Field, <<"=">>) || Field <- binary:split(Binary, <<"&">>)] of
    	[[_, ID], [_, Account]] -> {ID, Account};
    	NotExpected -> {malformed, NotExpected}
    end.

login(Binary) ->

	{ID, Account} = parse_form(Binary),

	case ets:lookup(players, ID) of
		[] -> ets:insert(players, {ID, Account}),
			  {received, <<"new_user">>};
		[Res] -> {received, jiffy:encode(Res)}
	end.