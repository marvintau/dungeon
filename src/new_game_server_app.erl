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

-type epgsql_type() :: atom() | {array, atom()} | {unknown_oid, integer()}.

-record(column, {
    name :: binary(),
    type :: epgsql_type(),
    size :: -1 | pos_integer(),
    modifier :: -1 | pos_integer(),
    format :: integer()
}).

% -record(statement, {
%     name :: string(),
%     columns :: [#column{}],
%     types :: [epgsql_type()]
% }).

start(_StartType, _StartArgs) ->


    cast_database:init_table(),

    Dispatch = cowboy_router:compile([
            {'_', [
                   {"/dungeon/[...]", cowboy_static, {priv_dir, new_game_server, "assets"}},
                   {"/battle", battle_handler, []},
                   {"/get_casts", cast_edit_handler, []},
                   {"/reset_casts", cast_reset_handler, []},
                   {"/get_list", cast_list_handler, []},
                   {"/post_casts", cast_submit_handler, []},
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