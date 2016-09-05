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
            primary     = {190, 235},
            secondary   = {0, 0},
            hp          = 3400,
            armor       = 5400,
            hit         = 15,
            critical    = 20,
            dodge       = 20,
            resistance  = 35,
            block       = 35,
            agility     = 50
        }),

    mnesia:dirty_write(#char{
            type        = mage,
            primary     = {255, 280},
            secondary   = {0, 0},
            hp          = 2300,
            armor       = 2700,
            hit         = 20,
            critical    = 35,
            dodge       = 20,
            resistance  = 15,
            block       = 0,
            agility     = 35
        }),

    mnesia:dirty_write(#char{
            type        = rogue,
            primary     ={190, 205},
            secondary   ={175, 190},
            hp          =2700,
            armor       =3100,
            hit         =25,
            critical    =45,
            dodge       =40,
            resistance  =20,
            block       =0 ,
            agility=75
        }),
    
        mnesia:dirty_write(#char{
            type        = hunter,
            primary     = {335, 370},
            secondary   = {0, 0},
            hp          = 3100,
            armor       = 5400,
            hit         = 35,
            critical    = 30,
            dodge       = 30,
            resistance  = 35,
            block       = 0 ,
            agility     = 40
        }),

    okay.

get_char(Type) ->
    [A] = mnesia:dirty_match_object(#char{type = Type, _ = '_'}),
    A.
