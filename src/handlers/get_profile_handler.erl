-module(get_profile_handler).

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

    error_logger:info_report(get_card_profile),

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            error_logger:error_report(Error),
            {<<"Nah">>, Req}
    end,

    erlang:display(ReqBody),

    {[{_, Id}]} = jiffy:decode(ReqBody),

    case is_list(Id)  of
        true ->
            IDs = string:join([ lists:concat(["'", binary_to_list(I), "'"]) || I <- Id], ", "),
            PayLoad = list_to_binary(["select * from character_card_profile where id in (", IDs, ")"]);
        _ -> 
            PayLoad = list_to_binary(["select * from character_card_profile where id='", Id,"'"])
        end,


    erlang:display(PayLoad),

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),



    Contents = case epgsql:squery(Conn, binary_to_list(PayLoad)) of
        {ok, _Cols, ResList} -> [ {[{card_id, ID}, {card_profile, jiffy:decode(Res)}]} || {ID, Res} <- ResList];
        E -> E
    end,


    ok = epgsql:close(Conn),

    Res = cowboy_req:set_resp_body(jiffy:encode(Contents), NextReq),
    {true, Res, State}.
