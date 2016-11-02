-module(login_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([allow_missing_posts/2]).
-export([allowed_methods/2]).
-export([handle_text/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Opts) ->
    {[<<"POST">>], Req, Opts}.

% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD request
% being processed.
content_types_accepted(Req, State) ->

    erlang:display(accepted),

    {[
        {<<"application/x-www-form-urlencoded">>, handle_text}
    ], Req, State}.

content_types_provided(Req, State) ->

    {[
        {<<"application/x-www-form-urlencoded">>, handle_text}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.


handle_text(Req, State) ->

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,
    

    {received, Respond} = player_database:login(ReqBody),
    Res = cowboy_req:set_resp_body(Respond, NextReq),
    {true, Res, State}.
