%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(battle_request_handler).

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

    {[{_, SelfPlayerId}, {_, OppoPlayerId}, {_, SelfSelectedCard}, {_, SelfSelectedSkills}]} = jiffy:decode(ReqBody),

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),

    SelfSelectedSkillsBinary = jiffy:encode(SelfSelectedSkills),

    UpdateSelfDefaultSkillsQuery = list_to_binary(["update player_profile 
        set profile=jsonb_set(profile, '{default_skills}', '", SelfSelectedSkillsBinary, "') where id='", SelfPlayerId, "';"]),

    UpdateSelfDefaultCardQuery = list_to_binary(["update player_profile set profile=jsonb_set(profile, '{default_card}', '\"", SelfSelectedCard, "\"') where id='", SelfPlayerId, "';"]),

    GetSelfDefaultSkillsQuery = list_to_binary(["select (profile ->>'default_skills')
        from player_profile where id='", SelfPlayerId ,"'"]),

    GetOppoDefaultSkillsQuery = list_to_binary(["select (profile ->>'default_skills')
        from player_profile where id='", OppoPlayerId ,"'"]),

    erlang:display({SelfPlayerId, OppoPlayerId, binary_to_list(UpdateSelfDefaultCardQuery)}),

    GetSelfCardProfile = list_to_binary(["select profile from character_card_profile where id in
        (select (profile ->>'default_card')::uuid from player_profile where id='", SelfPlayerId, "')"]),

    GetOppoCardProfile = list_to_binary(["select profile from character_card_profile where id in
        (select (profile ->>'default_card')::uuid from player_profile where id='", OppoPlayerId, "')"]),

    {ok, 1} = epgsql:squery(Conn, binary_to_list(UpdateSelfDefaultSkillsQuery)),
    {ok, 1} = epgsql:squery(Conn, binary_to_list(UpdateSelfDefaultCardQuery)),

    {ok, _, [{SelfSkills}]} = epgsql:squery(Conn, binary_to_list(GetSelfDefaultSkillsQuery)),
    {ok, _, [{OppoSkills}]} = epgsql:squery(Conn, binary_to_list(GetOppoDefaultSkillsQuery)),

    {ok, _, [{SelfCardProfile}]} = epgsql:squery(Conn, binary_to_list(GetSelfCardProfile)),
    {ok, _, [{OppoCardProfile}]} = epgsql:squery(Conn, binary_to_list(GetOppoCardProfile)),

    ok = epgsql:close(Conn),

    error_logger:info_report({battle, SelfPlayerId, challenges, OppoPlayerId}),

    {done, {records, Records}, {full_log, _}, {winner, Winner}} = battle:new({
        parse(jiffy:decode(SelfCardProfile, [return_maps]), SelfPlayerId, jiffy:decode(SelfSkills)),
        parse(jiffy:decode(SelfCardProfile, [return_maps]), OppoPlayerId, jiffy:decode(OppoSkills))}),

    Res = cowboy_req:set_resp_body(jiffy:encode({[{records, Records}, {winner, Winner}, {player1, jiffy:decode(SelfCardProfile)}, {player2, jiffy:decode(OppoCardProfile)}]}), NextReq),
    {true, Res, State}.


parse(SinglePlayerData, ID, Skills) ->

    #{<<"agi">>:=Agi,  <<"armor">>:=Armor, <<"block">>:=Block, <<"card_name">>:=_CardName,
      <<"cast_list">>:=_CastList,  <<"class">>:=_Class, <<"range_type">>:=RangeType, <<"critical">>:=Critic, <<"dodge">>:=Dodge, <<"hit">>:=HitBonus,
      <<"hp">>:=HP, <<"prim_max">>:=PrimMax, <<"prim_min">>:=PrimMin, <<"prim_type">>:=PrimType, <<"image_name">>:=_ImageName,
      <<"resist">>:=Resist, <<"secd_max">>:=SecdMax, <<"secd_min">>:=SecdMin, <<"secd_type">>:=SecdType,
      <<"talented_skill">>:=TalentedSkill} = SinglePlayerData,

    #{

        id         => ID,

        % State is the data that will be modified during a battle, and the result will
        % be preserved.

        state      => #{
            position   => 2,
            hp         => HP,
            rem_moves  => 0,
            diff => 0
        },

        done => already,

        range_type => RangeType,

        curr_hand  => {prim, binary_to_atom(PrimType, utf8), {PrimMin, PrimMax}},
        secd_hand  => {secd, binary_to_atom(SecdType, utf8), {SecdMin, SecdMax}},
        prim_hand  => {prim, binary_to_atom(PrimType, utf8), {PrimMin, PrimMax}},

        talented => binary_to_atom(TalentedSkill, utf8),
        casts => lists:map(fun(X) -> binary_to_atom(X, utf8) end, Skills),
        effects => [],

        orig_attr => #{
            diff => 0,
            attack_disabled => 0,
            cast_disabled => 0,
            effect_invalidated => 0,
            is_frozen => 0,
            is_stunned => 0,
            is_disarmed => 0,

            armor      => Armor,
            hit_bonus  => HitBonus,
            critical   => Critic,
            dodge      => Dodge,
            resist     => Resist,
            block      => Block,
            agility    => Agi,
            outcome    => null,
            damage_multiplier => 1,
            critical_multiplier => 1,
            damage_addon => 0,
            damage_taken => 0
        }
    }.
