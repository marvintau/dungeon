%%%-------------------------------------------------------------------
%% @doc new_game_server public API
%% @end
%%%-------------------------------------------------------------------

-module(new_game_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================


start(_StartType, _StartArgs) ->


    cast_database:init_table(),

    Dispatch = cowboy_router:compile([
            {'_', [
                   {"/dungeon/[...]", cowboy_static, {priv_dir, new_game_server, "assets"}},
                   {"/battle", battle_handler, []},
                   {"/getcasts", getcasts_handler, []},
                   {"/login", login_handler, []}
                  ]}
        ]),

    {ok, _} = cowboy:start_clear(my_http_listener, 100,
        [{port, 1334}],
        #{env => #{dispatch => Dispatch}}
    ),

    new_game_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
