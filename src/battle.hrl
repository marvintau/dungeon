
-type char_type() :: warrior | mage | rouge | hunter. 

-type range() :: { integer(), integer() }.

-type weapon() :: primary | secondary.

-record(char, {
        type        :: char_type(),
        primary     :: range(),
        secondary   :: range(),
        hp,
        armor,
        hit,
        critical,
        dodge,
        resistance,
        block,
        agility
    }).

-type character() :: #char{}. 

% =============== BATTLE CHARACTER CONTEXT ==========================

% Should record the current progress of battle, including current HP,
% remaining attacks during current move, and buffs or other effects
% currently applied on the character.

% The attributes might be changed during battle. A copy of original
% player data will be referred in context data structure, which used
% for recording the original or max limit of some attribute. this is
% not exepcted to be modified.

% Considering the accessing efficiency, the context should be stored
% in ETS table, and access with atomic operation, instead of creating
% copies in flying.

-record(player, {
        id          :: atom(),
        char        :: #char{},
        hp,
        atk_upper,
        atk_lower,
        armor,
        hit,
        critical,
        dodge,
        resistance,
        block,
        agility,
        remaining_attacks,
        weapon,
        status
    }).

-record(battle, {
        remaining_attacks}).
