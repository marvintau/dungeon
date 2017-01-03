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

                   {"/add_profile", add_profile_handler, []},
                   {"/get_profile", get_profile_handler, []},
                   {"/update_profile", update_profile_handler, []},

                   {"/get_player_list", get_player_list_handler, []},
                   {"/get_cast_names", get_casts_handler, []},

                   {"/check_chest", check_chest_handler, []},
                   {"/open_chest", open_chest_handler, []},

                   {"/reset_database", reset_database_handler, []}
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
