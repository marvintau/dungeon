-module(schema).

-author('Yue Marvin Tao').
-export([rebuild_schema/0, rebuild_table/0, get_char/1]).

-include("battle.hrl").

%% FOR SCHEMA
-define(RAM, {ram_copies, [node()]}).
-define(DISC, {disc_copies, [node()]}).
-define(TABLE_OPTIONS(Copies, Type, Fields), [Copies, {type, Type}, {attributes, Fields}]).


rebuild_schema() ->
    stopped = mnesia:stop(),

    case mnesia:delete_schema([node()]) of
        ok ->
            error_logger:info_report(schema_deleted),
        timer:sleep(300);
        _ ->
            error_logger:info_report("Most likely it doesn't exist.")
    end,


    case mnesia:create_schema([node()]) of
        ok ->
            error_logger:info_report(schema_created),
         timer:sleep(300);
         _ ->
            error_logger:info_report("Creating failed")
    end,

    ok = mnesia:start().



rebuild_table() ->
    {atomic, ok} = mnesia:create_table(char, ?TABLE_OPTIONS(?RAM, set, record_info(fields, char))),
    setup_char(),
    case mnesia:dirty_match_object(#char{type = warrior, _ = '_'}) of
        Res when length(Res) > 0 ->
            error_logger:info_report("Chararacter Table Validated.");
        _ ->
            error_logger:info_report("Oops.")
    end.


setup_char() ->
    mnesia:dirty_write(#char{
            type        = warrior,
            pri_type    = physical,
            primary     = {190, 235},
            sec_type    = shield,
            secondary   = {0, 0},
            hp          = 3400,
            armor       = 5400,
            hit         = 15,
            critic      = 20,
            dodge       = 20,
            resist      = 35,
            block       = 35,
            agility     = 50
        }),

    mnesia:dirty_write(#char{
            type        = mage,
            pri_type    = magic,
            primary     = {255, 280},
            sec_type    = bare,
            secondary   = {0, 0},
            hp          = 2300,
            armor       = 2700,
            hit         = 20,
            critic      = 35,
            dodge       = 20,
            resist      = 15,
            block       = 0,
            agility     = 35
        }),

    mnesia:dirty_write(#char{
            type        = rogue,
            pri_type    = physical,
            primary     = {190, 205},
            sec_type    = physical,
            secondary   = {175, 190},
            hp          = 2700,
            armor       = 3100,
            hit         = 25,
            critic      = 45,
            dodge       = 40,
            resist      = 20,
            block       = 0 ,
            agility=75
        }),
    
        mnesia:dirty_write(#char{
            type        = hunter,
            pri_type    = physical,
            primary     = {335, 370},
            sec_type    = bare,
            secondary   = {0, 0},
            hp          = 3100,
            armor       = 5400,
            hit         = 35,
            critic      = 30,
            dodge       = 30,
            resist      = 35,
            block       = 0 ,
            agility     = 40
        }),

    okay.

get_char(Type) ->
    [A] = mnesia:dirty_match_object(#char{type = Type, _ = '_'}),
    A.
