
-type char_type() :: warrior | mage | rouge | hunter. 

-type range() :: { integer(), integer() }.

-type weapon() :: primary | secondary.

-record(char, {
        type        :: char_type(),
        pri_type    :: {damage, physical} | {damage, magic},
        primary     :: range(),
        sec_type    :: {damage, physical} | {damage, magic} | {no_damage, shield} | {no_damage, bare},
        secondary   :: range(),
        hp,
        armor,
        hit,
        critic,
        dodge,
        resist,
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
        critic,
        dodge,
        resist,
        block,
        agility,
        which_hand,
        status
    }).

-record(battle, {
        seq_no,
        outcome :: plain | critical | dodge | block | resist, 
        damage,
        log_type,
        is_latter,
        rem_atk}).
