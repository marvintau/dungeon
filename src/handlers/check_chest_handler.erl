-module(check_chest_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([allow_missing_posts/2]).
-export([allowed_methods/2]).
-export([handle_post/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Opts) ->
    {[<<"POST">>], Req, Opts}.

content_types_accepted(Req, State) ->

    {[
        {<<"application/text">>, handle_post},
        {<<"application/json">>, handle_post}
    ], Req, State}.


% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD reQuery
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.


handle_post(Req, State) ->

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,


    {[{_, ID}]} = jiffy:decode(ReqBody),

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),

    Query = list_to_binary(["select
                char_id, last_opened_chest, chest_name, open_interval, last_opened_time
            from
                char_chest
                inner join chest_spec on char_chest.last_opened_chest%4+1 = chest_spec.chest_id
                where
            char_id = '", ID, "';"]),


    {ok, _Cols, Contents} = epgsql:squery(Conn, binary_to_list(Query)),


    [{ID, NextChestID, NextName, Interval, LastOpen}] = Contents,

    RawJsonContent = {[{id, ID}, {next_chest, NextChestID}, {next_name, NextName}, {intv, Interval}, {last_time, LastOpen}]},

    ok = epgsql:close(Conn),

    Res = cowboy_req:set_resp_body(jiffy:encode(RawJsonContent), NextReq),
    {true, Res, State}.
