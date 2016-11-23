-module(battle_player_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local, battle_player_server}, battle_player_server, [], []).

init(PlayerInfoBinary) ->
    {ok, PlayerInfoBinary}.

handle_call({get, Attr}, _From, Args) ->
    ok.
