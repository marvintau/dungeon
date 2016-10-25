-module(cast_edit_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_accepted(Req, State) ->
    erlang:display(accepted),

    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.


% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD request
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


handle_get(Req, State) ->

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,
    

    Data = jiffy:decode(ReqBody),
    error_logger:info_report(Data),
    {done, ResBody} = battle_db:list_cast_json(Data),
    Res = cowboy_req:set_resp_body(ResBody, NextReq),
    {true, Res, State}.
