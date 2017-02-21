-module(add_new_player_handler).

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


random_name() ->
    SurNames = [{1, <<"威猛的"/utf8>>}, {2, <<"霸道的"/utf8>>}, {3, <<"机智的"/utf8>>}, {4, <<"勇敢的"/utf8>>}, {5, <<"无敌的"/utf8>>}, {6, <<"可爱的"/utf8>>}, {7, <<"呆萌的"/utf8>>}, {8, <<"天然的"/utf8>>}, {9, <<"干练的"/utf8>>}, {10, <<"飒爽的"/utf8>>}],
    Given = [{1, <<"总裁"/utf8>>}, {2, <<"战士"/utf8>>}, {3, <<"魔法使"/utf8>>}, {4, <<"隐士"/utf8>>}, {5, <<"高人"/utf8>>}, {6, <<"科学家"/utf8>>}, {7, <<"学霸"/utf8>>}, {8, <<"天才"/utf8>>}, {9, <<"学生会长"/utf8>>}, {10, <<"体育部长"/utf8>>}],

    {_, [{_, ResSurname}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(SurNames) end, SurNames),
    {_, [{_, ResGiven}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(Given) end, Given),

    list_to_binary([ResSurname,ResGiven]).

handle_post(Req, State) ->

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),


    quickrand:seed(),
    IDstring = uuid:uuid_to_string(uuid:get_v4_urandom()),
    Name = random_name(),

    QueryCheckNumberOfIdenticalNames = list_to_binary(["select count(profile->'player_name') from player_profile where profile->>'player_name' like '", Name, "%'"]),
    {ok, _, [{Numbers}]}= epgsql:squery(Conn, binary_to_list(QueryCheckNumberOfIdenticalNames)),

    NewName = case Numbers of 
        <<"0">> -> Name;
        More -> list_to_binary([Name, integer_to_binary(binary_to_integer(More))])
    end,

    QueryAddProfile = list_to_binary([
        "insert into player_profile (id, profile) values ('",
        IDstring,
        "', '{\"player_name\": \"",
        NewName,
        "\", \"player_level\":1,
        \"default_card\": \"946ae77c-183b-4538-b439-ac9036024676\",
        \"card_list\":[\"946ae77c-183b-4538-b439-ac9036024676\",
            \"15d715a8-d585-48fc-a65a-286fc41c9a3f\",
            \"a0c1a883-2995-4526-856c-26870e5b3f74\",
            \"be2d65f0-3c93-457e-8180-de7c93a365a5\",
            \"a009e5e9-2057-4353-9871-309d68752c1b\",
            \"db1c75ca-aa32-4f2b-9bb1-355267d4a2ad\",
            \"849d31be-b9cd-494c-9ccd-7cc656153b57\",
            \"1b0cf5e0-2164-46fd-8424-2146fca99fb9\"]}')"
    ]),

    erlang:display(QueryAddProfile),

    QueryAddChestOpening = list_to_binary([
        "insert into char_chest(char_id, last_opened_chest, last_opened_time) values ('", IDstring, "', '1', now())"
    ]),

    AddProfileRes = epgsql:squery(Conn, binary_to_list(QueryAddProfile)),
    error_logger:info_report(AddProfileRes),

    AddChestRes = epgsql:squery(Conn, binary_to_list(QueryAddChestOpening)),
    error_logger:info_report(AddChestRes),

    ok = epgsql:close(Conn),

    erlang:display({new_player, IDstring, created}),

    Res = cowboy_req:set_resp_body(list_to_binary(["{\"id\":\"",IDstring, "\"}"]), NextReq),
    {true, Res, State}.
