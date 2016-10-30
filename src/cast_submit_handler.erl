%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(cast_submit_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([allow_missing_posts/2]).
-export([allowed_methods/2]).
-export([handle_casts_submit/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Opts) ->
    {[<<"POST">>], Req, Opts}.

content_types_accepted(Req, State) ->

    {[
        {<<"application/json">>, handle_casts_submit}
    ], Req, State}.


% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD request
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_casts_submit}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.

handle_casts_submit(Req, State) ->
    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            error_logger:info_report(ReqBodyRaw),
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    Data = jiffy:decode(ReqBody),
    ok = battle_db:update_cast(Data),
    {true, NextReq, State}.
