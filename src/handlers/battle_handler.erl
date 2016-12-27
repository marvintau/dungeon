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

    {ok, Conn} = epgsql:connect("localhost", "yuetao", "asdasdasd", [
        {database, "dungeon"},
        {timeout, 100}
    ]),

    ProfileQuery1 = list_to_binary(["select * from player_profile where id='", Id1,"'"]),

    ProfileQuery2 = list_to_binary(["select * from player_profile where id='", Id2, "'"]),

    {ok, _Cols, [{_, Profile1}]} = epgsql:squery(Conn, binary_to_list(ProfileQuery1)),

    {ok, _Cols, [{_, Profile2}]} = epgsql:squery(Conn, binary_to_list(ProfileQuery2)),

    ok = epgsql:close(Conn),

    {done, ResBody} = battle:new({
        parse(jiffy:decode(Profile1, [return_maps])),
        parse(jiffy:decode(Profile2, [return_maps]))
    }),

    Res = cowboy_req:set_resp_body(ResBody, NextReq),
    {true, Res, State}.


parse(SinglePlayerData) ->

    #{<<"agi">>:=Agi, <<"armor">>:=Armor, <<"block">>:=Block, <<"cast_list">>:=CastList,
      <<"class">>:=Class, <<"critical">>:=Critic, <<"dodge">>:=Dodge, <<"hit">>:=HitBonus,
      <<"hp">>:=HP, <<"id">>:=ID, <<"prim_max">>:=PrimMax, <<"prim_min">>:=PrimMin, <<"prim_type">>:=PrimType,
      <<"resist">>:=Resist, <<"secd_max">>:=SecdMax, <<"secd_min">>:=SecdMin, <<"secd_type">>:=SecdType,
      <<"talented_skill">>:=TalentedSkill} = SinglePlayerData,

    #{

        id         => binary_to_atom(ID, utf8),

        % State is the data that will be modified during a battle, and the result will
        % be preserved.

        state      => #{
            hp         => HP,
            rem_moves  => 0,
            diff => 0
        },

        done => already,

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