-module(get_casts_handler).

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

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,


    Data = jiffy:decode(ReqBody),
    {done, ResBody} = get_cast_names_with(Data),
    Res = cowboy_req:set_resp_body(ResBody, NextReq),
    {true, Res, State}.

get_cast_names_with(Data) ->
    {[{<<"id">>, _ID}, {<<"class">>, Class}]} = Data,

    General = lists:flatten(ets:match(casts, {'$1', general, '_'})),
    ClassCast = lists:flatten(ets:match(casts, {'$1', binary_to_atom(Class, utf8), '_'})),

    FullNameList = lists:append([[none], ClassCast, General]),
    {done, jiffy:encode(FullNameList)}.
