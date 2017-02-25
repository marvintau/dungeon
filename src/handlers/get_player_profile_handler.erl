-module(get_player_profile_handler).

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
% specified here will be only called when GET and HEAD request
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.


handle_post(Req, State) ->

    error_logger:info_report({request, self, player, profile}),

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            error_logger:error_report(Error),
            {<<"Nah">>, Req}
    end,


    {[{_, Id}]} = jiffy:decode(ReqBody),

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),

    PayLoad = list_to_binary(["select player_profile.profile || jsonb_build_object('card_head', character_card_profile.profile->>'image_name')
        from player_profile join character_card_profile on (player_profile.profile->>'default_card' = character_card_profile.id::text)
        where player_profile.id='",Id, "'"]),

    {ok, _Cols, [{Contents}]} = epgsql:squery(Conn, binary_to_list(PayLoad)),

    ok = epgsql:close(Conn),

    Res = cowboy_req:set_resp_body(Contents, NextReq),
    {true, Res, State}.
