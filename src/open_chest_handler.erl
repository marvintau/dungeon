-module(open_chest_handler).

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

    QueryUpdate = list_to_binary(["update char_chest
        set
            last_opened_chest = last_opened_chest % 4 + 1,
            last_opened_time = now()
        where char_id = '", ID, "';"
    ]),

    QueryGetDroppedItemTypes = list_to_binary(["
        select chest_id, round(random() * (max_item_types - min_item_types)) + min_item_types as item_types
        from
            char_chest
        inner join chest_spec on char_chest.last_opened_chest = chest_spec.chest_id
        where char_id = '", ID, "';
        "
    ]),

    {ok, 1} = epgsql:squery(Conn, binary_to_list(QueryUpdate)),

    {ok, _, [{ChestID, DroppedNumberBin}]} = epgsql:squery(Conn, binary_to_list(QueryGetDroppedItemTypes)),
    DroppedNumber = binary_to_integer(DroppedNumberBin),
    error_logger:info_report({ChestID, DroppedNumber}),

    QueryListPossibleItems = list_to_binary(["

        select item_id, item_name, items
        FROM
        (SELECT
            tem.item_id, item_name, items, generate_series(1, drop_rate/5) as nah
        from
            (select
                chest_id, item_id, drop_rate,
                round(random() * (max_items - min_items) + min_items) as items
            from
                item_from_chest
            where chest_id='", ChestID, "'
            ) as tem
        inner join
            item_description on tem.item_id = item_description.item_id) as populated
        group by item_id, item_name, items
        order by random()"
    ]),

    {ok, _, PossibleDroppedItem} = epgsql:squery(Conn, binary_to_list(QueryListPossibleItems)),
    RawJsonContent = [{[{id, ItemID}, {name, ItemName}, {num, ItemNum}]} || {ItemID, ItemName, ItemNum} <- lists:sublist(PossibleDroppedItem, DroppedNumber)],

    ok = epgsql:close(Conn),

    Res = cowboy_req:set_resp_body(jiffy:encode(RawJsonContent), NextReq),
    {true, Res, State}.
