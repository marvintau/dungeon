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

    % 提交一个query，返回用户id，下一次将要打开的chest类型，此类型chest需要等待的时间，以及上次开chest的时间

    error_logger:info_report({incoming_request_check_chest, Req}),

    QueryCheck = list_to_binary(["select
                char_id, last_opened_chest % 5 + 1, chest_name,
                interval '1s' * open_interval - (now() - last_opened_time) as remaining,
                extract(epoch from last_opened_time) * 100000 as last_opened_time, is_today_done
            from
                char_chest
                inner join chest_spec on char_chest.last_opened_chest % 5 + 1 = chest_spec.chest_id
                where
            char_id = '", ID, "';"]),


    {ok, _Cols, Contents} = epgsql:squery(Conn, binary_to_list(QueryCheck)),
    
    error_logger:info_report(Contents),
    [{ID, NextChestID, NextName, Remaining, LastOpen, IsTodayDone}] = Contents,

    % 检查此次开箱子请求是否和记录的上一次开箱子在同一天
    RawJsonContent = case is_same_day(LastOpen) of
        % 如果同一天，OK
        {true, _} ->
            error_logger:info_report({open_chest_at_same_day, {is_today_done, IsTodayDone}}),
            {[{id, ID}, {next_chest, NextChestID}, {next_name, NextName}, {remaining, Remaining}, {is_today_done, IsTodayDone}]};

        % 如果不在同一天
        _ ->

            % 在这里提交一个query，把此用户上次开的箱子变成0（但实际上不会轮到它开），上次开的时间变成今天0时
            % 由于发起QueryCheck返回的是下一个要打开的箱子类型，所以确保返回的下一个要打开的箱子ID仍然是1
            QueryReset= list_to_binary(["update char_chest
                set
                    last_opened_chest = 0,
                    last_opened_time = now() + age(now()),
                    is_today_done = 'no'
                where char_id = '", ID, "';"
            ]),

            % 再重新发起一次check的请求，
            {ok, 1} = epgsql:squery(Conn, binary_to_list(QueryReset)),
            error_logger:info_report({open_at_next_day, reset}),

            {ok, _Cols, UpdatedContents} = epgsql:squery(Conn, binary_to_list(QueryCheck)),
            [{ID, NextChestID0, NextName0, Remaining0, _, IsTodayDone}] = UpdatedContents,
            {[{id, ID}, {next_chest, NextChestID0}, {next_name, NextName0}, {remaining, Remaining0}, {is_today_done, IsTodayDone}]}
        end,

    ok = epgsql:close(Conn),

    Res = cowboy_req:set_resp_body(jiffy:encode(RawJsonContent), NextReq),
    {true, Res, State}.

is_same_day(<<Mega:4/binary, Sec:6/binary, MilliSec/binary>>) ->

    LastTimeStamp = {binary_to_integer(Mega), binary_to_integer(Sec), binary_to_integer(MilliSec)},
    {LastDate, {LastHour, LastMin, _}} = calendar:now_to_datetime(os:timestamp()),
    {CurrDate, {CurrHour, CurrMin, _}} = calendar:now_to_datetime(LastTimeStamp),
    error_logger:info_report({LastDate, CurrDate, LastHour, CurrHour}),

    {(LastDate == CurrDate) , CurrDate}.
