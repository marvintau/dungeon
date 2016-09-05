-module(battle).

-author('Yue Marvin Tao').

-include("battle.hrl").

-export([init_database/0, init_new_battle/1 ]).

mage_damage(Random, Outcome, Upper, Lower) ->
    
    % ------------------- PLAYER AS MAGE --------------------------
    % Magic attack cannot be blocked, thus make sure that block has
    % been set 0 before turning the roulette.

    case Outcome of
        dodge ->
            0;
        resist ->
            round(Random * Lower / 10);
        critical ->
            round((Lower + Random * (Upper - Lower)) * 2);
        _ ->
            round(Lower + Random * (Upper - Lower))
    end.

non_mage_damage(Random, Outcome, Upper, Lower, Armor) ->
    
    % --------------- PLAYER AS NON-MAGE --------------------------
    % Physical attack cannot be resisted, make sure that resist has
    % been ser 0 before turning the roulette.

    case Outcome of
        dodge ->
            0;
        block ->
            0;
        critical ->
            round((Lower + Random * (Upper - Lower)) * 2 * (1 - Armor * 0.0001));
        _ ->
            round(Lower + Random * (Upper - Lower) * (1 - Armor * 0.0001))
    end.


% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

% Calculates the damage with given character type, the upper and lower
% damage of weapon, and outcome of roulette turning.

single_attack(CharType, Upper, Lower, Armor, Outcome) ->

    Random = random:uniform(),

    FinalDamage = case CharType of

        mage ->
            mage_damage(Random, Outcome, Upper, Lower);
        _ ->
            non_mage_damage(Random, Outcome, Upper, Lower, Armor)
    end,

    FinalDamage.

% ============ SINGLE ATTACK CALCULATED FROM CONTEXT ==================

single_attack(Attack, Defense, Outcome) ->

    single_attack(
        Attack#context.char#char.type,
        Attack#context.atk_upper,
        Attack#context.atk_lower,
        Defense#context.armor,
        Outcome).


% ======================= MAIN BATTLE LOOP ============================

% In the future, both PlayerID and Char information should be extracted
% from player database.

context_from_player(PlayerID, Char) ->
    #context{
        id         = PlayerID,
        char       = Char,
        hp         = Char#char.hp,
        armor      = Char#char.armor,
        hit        = Char#char.hit,
        critical   = Char#char.critical,
        dodge      = Char#char.dodge,
        resistance = Char#char.resistance,
        block      = Char#char.block,
        agility    = Char#char.agility,
 
        remaining_attacks = 2,
        weapon = primary,
        atk_upper  = element(2, Char#char.primary),
        atk_lower  = element(1, Char#char.primary)
    }.

battle_loop({Player1ID, Char1}, {Player2ID, Char2}, LogType) ->

    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),

    Player1Context = context_from_player(Player1ID, Char1),
    Player2Context = context_from_player(Player2ID, Char2),

    case Player1Context#context.agility > Player2Context#context.agility of
        true ->
            battle_loop(Player1Context, Player2Context, <<>>, 0, LogType);
        _ ->
            battle_loop(Player2Context, Player1Context, <<>>, 0, LogType)
    end.


get_next_attack_context(Remaining, Attack) ->

    {NextWeapon, {NextAttackUpper, NextAttackLower}} = case Attack#context.weapon of
        primary ->
            {secondary, Attack#context.char#char.secondary};
        _ ->
            {primary, Attack#context.char#char.primary}
    end,

    Attack#context{
        remaining_attacks = Remaining - 1,
        atk_upper = NextAttackUpper,
        atk_lower = NextAttackLower,
        weapon = NextWeapon
    }.


battle_loop(Attack, Defense, Log, SequenceNumber, LogType) when Attack#context.hp > 0 andalso Defense#context.hp > 0 ->

    % Anything that might change the battle context should be placed here.

    case Attack#context.remaining_attacks of
        Any when Any > 0 ->
            

            Outcome = roulette:turn_roulette(roulette:prepare_roulette(Attack, Defense)),
            
            NextAttack = get_next_attack_context(Any, Attack),
           
            DefenseHP = Defense#context.hp,
            DamageDealt = single_attack(Attack, Defense, Outcome),
            NextDefense = Defense#context{hp = DefenseHP - DamageDealt},

            NewLog = case LogType of
                json ->
                    list_to_binary([ "{",
                    "seq:", integer_to_list(SequenceNumber), ", "
                    "attacker: \"", atom_to_list(Attack#context.id), "\", "
                    "outcome: \"", atom_to_list(Outcome), "\", "
                    "damage:", integer_to_list(DamageDealt), ", "
                    "attacker_hp:", integer_to_list(Attack#context.hp), ", "
                    "defenser_hp:", integer_to_list(DefenseHP), 
                    "}, "]);
                html ->
                    list_to_binary([
                    "<b>Sequence: </b>", integer_to_list(SequenceNumber), "\t\t ",
                    "<b>Attacker: </b>", atom_to_list(Attack#context.id)," ",
                                         atom_to_list(Attack#context.char#char.type), "\t\t ",
                    "<b>Action: </b>: ", atom_to_list(Outcome), "\t\t ",
                    "<b>Damage: </b>", integer_to_list(DamageDealt), "\t\t ",
                    "<b>Attacker's HP: </b>", integer_to_list(Attack#context.hp), "\t\t ",
                    "<b>Defenser's HP: </b>", integer_to_list(DefenseHP), 
                    "<br> "])
            end,


            battle_loop(NextAttack, NextDefense, <<Log/binary, NewLog/binary>>, SequenceNumber, LogType);
        
        _ ->

            case Defense#context.remaining_attacks of 

                0 ->
                    
                    case Attack#context.agility > Defense#context.agility of
                        true ->
                            battle_loop(
                                Attack#context{remaining_attacks = 2},
                                Defense#context{remaining_attacks = 2},
                                Log,
                                SequenceNumber + 1,
                                LogType
                            );
                        _ ->
                            battle_loop(
                                Defense#context{remaining_attacks = 2},
                                Attack#context{remaining_attacks = 2},
                                Log,
                                SequenceNumber + 1,
                                LogType
                            )
                    end;

                _ ->
                    battle_loop(Defense, Attack, Log, SequenceNumber, LogType)
            end
    end;

battle_loop(_Attack, _Defense, Log, _, LogType) ->
    case LogType of
        html ->
            {done, Log};
        json ->
            {done, <<"{done : [", Log/binary ,"]}">>}
    end.

init_database() ->
    schema:rebuild_schema(),
    schema:rebuild_table().

random_char() ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    
    schema:get_char(lists:nth(trunc(random:uniform()*4)+1, [warrior, hunter, mage, rogue])).


init_new_battle(LogType) ->
    error_logger:info_report("Initiates new battle."),
    battle_loop({alice, random_char()}, {bob, random_char()}, LogType).
