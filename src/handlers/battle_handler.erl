%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(battle_handler).

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

    {[{_, Id1}, {_, Id2}]} = jiffy:decode(ReqBody),

    {ok, Conn} = epgsql:connect("localhost", "marvin", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),

    ProfileQuery1 = list_to_binary(["select * from character_card_profile where id='", Id1,"'"]),

    ProfileQuery2 = list_to_binary(["select * from character_card_profile where id='", Id2, "'"]),

    {ok, _Cols, [{_, Profile1}]} = epgsql:squery(Conn, binary_to_list(ProfileQuery1)),

    {ok, _Cols, [{_, Profile2}]} = epgsql:squery(Conn, binary_to_list(ProfileQuery2)),

    ok = epgsql:close(Conn),

    {done, {records, Records}, {full_log, FullLogs}, {winner, Winner}} = battle:new({
        parse(jiffy:decode(Profile1, [return_maps])),
        parse(jiffy:decode(Profile2, [return_maps]))
    }),

    Res = cowboy_req:set_resp_body(jiffy:encode({[{records, Records}, {winner, Winner}, {full_log, FullLogs}]}), NextReq),
    {true, Res, State}.


parse(SinglePlayerData) ->

    erlang:display(SinglePlayerData),

    % #{<<"agi">>=>75,<<"armor">>=>4500,  <<"block">>=>0,     <<"card_name">>=><<230,153,174,233,128,154,229,136,186,229,174,162>>,
    % <<"cast_list">>=>[<<"poison_gas">>,<<"talisman_of_spellshrouding">>,<<"talisman_of_death">>,<<"holy_hand_grenade">>,<<"none">>],
    % <<"class">>=><<"rogue">>,<<"critical">>=>30,<<"dodge">>=>30,<<"hit">>=>35,<<"hp">>=>2700,<<"image_name">>=><<"normal_rogue">>,
    % <<"prim_max">>=>205,<<"prim_min">>=>190,<<"prim_type">>=><<"physical">>,<<"range_type">>=><<"near">>,<<"resist">>=>35,
    % <<"secd_max">>=>190,<<"secd_min">>=>175,<<"secd_type">>=><<"physical">>,<<"talented_skill">>=><<"blade_dance">>}

    % {<<"agi">>=>40,<<"armor">>=>4500,<<"block">>=>0,<<"card_name">>=><<231,151,180,229,145,134,231,140,142,228,186,186>>,
    % <<"cast_list">>=>[<<"poison_gas">>,<<"talisman_of_spellshrouding">>,<<"talisman_of_death">>,<<"holy_hand_grenade">>,<<"none">>],
    % <<"class">>=><<"hunter">>,<<"critical">>=>30,<<"dodge">>=>30,<<"hit">>=>35,<<"hp">>=>3400,<<"prim_max">>=>3700,<<"prim_min">>=>335,<<"prim_type">>=><<"physical">>,<<"range_type">>=><<"far">>,<<"resist">>=>35,<<"secd_max">>=>100,<<"secd_min">>=>50,<<"secd_type">>=><<"bare">>,<<"talented_skill">>=><<"blade_dance">>}

    #{<<"agi">>:=Agi,  <<"armor">>:=Armor, <<"block">>:=Block, <<"card_name">>:=CardName,
      <<"cast_list">>:=CastList,  <<"class">>:=Class, <<"range_type">>:=RangeType, <<"critical">>:=Critic, <<"dodge">>:=Dodge, <<"hit">>:=HitBonus,
      <<"hp">>:=HP, <<"prim_max">>:=PrimMax, <<"prim_min">>:=PrimMin, <<"prim_type">>:=PrimType, <<"image_name">>:=_ImageName,
      <<"resist">>:=Resist, <<"secd_max">>:=SecdMax, <<"secd_min">>:=SecdMin, <<"secd_type">>:=SecdType,
      <<"talented_skill">>:=TalentedSkill} = SinglePlayerData,

    #{

        id         => CardName,

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
        casts => lists:map(fun(X) -> binary_to_atom(X, utf8) end, CastList),
        effects => [],

        orig_attr => #{
            diff => 0,
            attack_disabled => 0,
            cast_disabled => 0,
            effect_invalidated => 0,
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
