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

    battle_db:init_table(),

    Dispatch = cowboy_router:compile([
            {'_', [
                   {"/exam/[...]", cowboy_static, {priv_dir, new_game_server, "assets"}},
                   {"/get_result", new_game_handler, []},
                   {"/get_casts", cast_edit_handler, []},
                   {"/get_list", cast_list_handler, []},
                   {"/post_casts", cast_submit_handler, []},
                   {"/remove_cast", cast_remove_handler, []}
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
