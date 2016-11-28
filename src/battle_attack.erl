-module(battle_attack).
-author('Yue Marvin Tao').

-export([attack/4, attack_effected/4, bin/2]).

% ------------------------ ROTATE ROULETTE ----------------------------
% get the random choice result according to the probability of each 
% option.

bin(GivenVal, [Bin|Bins]) -> bin(GivenVal, Bin, Bins, 1).
bin(_, _, [], Ith) -> Ith;
bin(GivenVal, Accum, _, Ith) when GivenVal < Accum -> Ith;
bin(GivenVal, Accum, [Bin|Bins], Ith) -> bin(GivenVal, Accum+Bin, Bins, Ith+1).

rotate(Roulette, Rand) ->

    element(bin(Rand * 120, Roulette), {attack, dodge, resist, block, critical}).


% ----------------------- PREPARE ROULETTE ----------------------------
% Roulette is generated from the player's attribute, the current weapon
% of current player in move, and the secondary weapon of the defender.
% Magic attack is not dodgable and blockable, Physical attack might be
% blocked if the defender is carrying a shield, or can be only dodged
% otherwise.

% Notably, the MAX_LIMIT is the max size of the Roulette. excluding the
% part of Dodge, Resist, Block and Critical, the remaining part will left
% for plain attack.

prepare_roulette_from(
    #{curr_hand:={_, Curr, _}, attr:=#{hit_bonus:=Hit, critical:=Critical}},
    #{secd_hand:={_, Secd, _}, attr:=#{resist:=Res, block:=Blo, dodge:=Dod}}
) ->

    MAX_LIMIT = 120,

    {Dodge, Resist, Block} = case {Curr, Secd} of
        
        {magic, _} ->
            {0, Res, 0};
        
        {physical, shield} ->
            {Dod, 0, Blo};

        {physical, _} ->
            {Dod, 0, 0};
        _ ->             
            {0, 0, 0}            
    end,

    NewDodge = case Dodge - Hit > 0 of 
        true -> Dodge - Hit;
        _ -> 0
    end,

    [MAX_LIMIT - NewDodge - Resist - Block - Critical, NewDodge, Resist, Block, Critical].

% --------------- PLAYER AS MAGE ------------------------------
% Magic attack cannot be blocked, thus make sure that block has
% been set 0 before turning the roulette.

calculate_damage(magic, resist, {Lower, _}, _Armor, Rand) ->
    round(Rand * Lower / 10);

calculate_damage(magic, critical, {Lower, Upper}, _Armor, Rand) ->
    round(2*(Lower + Rand * (Upper - Lower)));

calculate_damage(magic, _, {Lower, Upper}, _Armor, Rand) ->
    round(Lower + Rand * (Upper - Lower));

calculate_damage(physical, dodge, _, _, _) -> 0;
calculate_damage(physical, block, _, _, _) -> 0;
calculate_damage(physical, critical, {Lower, Upper}, Armor, Rand) ->
    round(2*(Lower + Rand * (Upper - Lower)) * (1 - Armor * 0.0001));

calculate_damage(physical, _, {Lower, Upper}, Armor, Rand) ->
    round((Lower + Rand * (Upper - Lower)) * (1 - Armor * 0.0001));

calculate_damage(_, _, _, _, _) -> 0.

% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

is_no_damage_move(#{curr_hand:={secd, bare, _}})-> true;
is_no_damage_move(#{curr_hand:={secd, shield, _}})-> true;
is_no_damage_move(_) -> false.

log(#{seq:=Seq, stage:=Stage, mover:=Mover},
    #{state:=#{hp:=HpO}, curr_hand:={Which, WeaponType, _}},
    #{id:=IdD, state:=#{hp:=HpD}, attr:=#{outcome:=Outcome, damage_taken:=Damage}})  ->
    
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, IdD},
        { hand, Which}, { action, WeaponType},
        { outcome, Outcome }, { damage, Damage },
        { offender_hp, HpO}, { defender_hp, HpD}
    ]}.

attack(S,
       #{curr_hand:={HandType, AttackType, DamageRange}, prim_hand:=PrimHand, secd_hand:=SecdHand,
         attr:=#{damage_multiplier:=DamageMul, critical_multiplier:=CritMul, damage_addon:=DamageAddon},
         state:=#{rem_moves:=RemMoves}=StateA}=A,
       #{attr:=#{armor:=Armor}=CurrAttrD, state:=#{hp:=H2}=StateD}=D, L) ->

    S0 = rand:seed_s(exsplus),
    {Rand1, S1} = rand:uniform_s(S0),
    {Rand2, _} = rand:uniform_s(S1),

    Outcome = rotate(prepare_roulette_from(A, D), Rand1),
    
    Damage = calculate_damage(AttackType, Outcome, DamageRange, Armor, Rand2),

    AddedDamage = case Outcome of
        critical -> Damage * CritMul + DamageAddon;
        attack -> Damage * DamageMul + DamageAddon;
        _ -> Damage
    end,

    NextA = case HandType of
        prim -> A#{curr_hand:=SecdHand};
        secd -> A#{curr_hand:=PrimHand}
    end,
    NextD = D#{attr:=CurrAttrD#{damage_taken:=AddedDamage, outcome:=Outcome}, state:=StateD#{hp:=H2 - AddedDamage}},


    NextLog = case is_no_damage_move(A) of
        true -> L;
        _ -> [log(S, A, NextD) | L]
    end,
   
    {NextA#{state:=StateA#{rem_moves:=RemMoves-1}}, NextD, NextLog}.


attack_effected(State, #{attr:=#{attack_disabled:=0}}=O, D, Log) ->

    {#{state:=#{rem_moves:=RemMovesO}}=MovedO, MovedD, MovedLog} = battle_attack:attack(State, O, D, Log),

    DoneMovedO = case RemMovesO of
        0 -> MovedO#{done:=already};
        _ -> MovedO
    end,
    
    {DefReactedD, DefReactedO, DefReactedLog} = battle_effect:effect(State, MovedD, DoneMovedO, MovedLog),
    battle_effect:effect(State, DefReactedO, DefReactedD, DefReactedLog);

attack_effected(_State, #{state:=StateO}=O, D, Log) ->
    {O#{done:=already, state:=StateO#{rem_moves:=0}}, D, Log}.
