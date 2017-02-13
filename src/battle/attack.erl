-module(attack).
-author('Yue Marvin Tao').

-export([apply/4, bin/2]).



rand() -> element(3, erlang:timestamp())/1000000.

% ------------------------ ROTATE ROULETTE ----------------------------
% get the random choice result according to the probability of each 
% option.

bin(GivenVal, [Bin|Bins]) -> bin(GivenVal, Bin, Bins, 1).
bin(_, _, [], Ith) -> Ith;
bin(GivenVal, Accum, _, Ith) when GivenVal < Accum -> Ith;
bin(GivenVal, Accum, [Bin|Bins], Ith) -> bin(GivenVal, Accum+Bin, Bins, Ith+1).


% ----------------------- PREPARE ROULETTE ----------------------------
% Roulette is generated from the player's attribute, the current weapon
% of current player in move, and the secondary weapon of the defender.
% Magic attack is not dodgable and blockable, Physical attack might be
% blocked if the defender is carrying a shield, or can be only dodged
% otherwise.

% Notably, the MAX_LIMIT is the max size of the Roulette. excluding the
% part of Dodge, Resist, Block and Critical, the remaining part will left
% for plain attack.

roulette(
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

    Roulette = [MAX_LIMIT - NewDodge - Resist - Block - Critical, NewDodge, Resist, Block, Critical],

    Binned = bin(rand:uniform() * 120, Roulette),
    Result = element(Binned, {attack, dodge, resist, block, critical}),
    Result.

% --------------- PLAYER AS MAGE ------------------------------
% Magic attack cannot be blocked, thus make sure that block has
% been set 0 before turning the roulette.

calculate_damage(magic, resist, {Lower, _}, _Armor) ->
    round(rand() * Lower / 10);

calculate_damage(magic, critical, {Lower, Upper}, _Armor) ->
    round(2*(Lower + rand() * (Upper - Lower)));

calculate_damage(magic, _, {Lower, Upper}, _Armor) ->
    round(Lower + rand() * (Upper - Lower));

calculate_damage(physical, dodge, _, _) -> 0;
calculate_damage(physical, block, _, _) -> 0;
calculate_damage(physical, critical, {Lower, Upper}, Armor) ->
    round(2*(Lower + rand() * (Upper - Lower)) * (1 - Armor * 0.0001));

calculate_damage(physical, _, {Lower, Upper}, Armor) ->
    round((Lower + rand() * (Upper - Lower)) * (1 - Armor * 0.0001));

calculate_damage(_, _, _, _) -> 0.

% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

is_no_damage_move(#{curr_hand:={secd, bare, _}})-> true;
is_no_damage_move(#{curr_hand:={secd, shield, _}})-> true;
is_no_damage_move(_) -> false.

log(#{seq:=Seq, stage:=Stage, mover:=Mover},
    #{state:=#{hp:=HpO}, curr_hand:={WhichHand, _, _}},
    #{state:=#{hp:=HpD}, attr:=#{outcome:=Outcome, damage_taken:=Damage}},
    OffPosAct, DefPosAct, PosO, PosD)  ->
    
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { action, list_to_binary(lists:append([atom_to_list(Outcome), "_", atom_to_list(WhichHand)]))},
        { effects, []}, { damage, Damage },
        { offenderHP, HpO}, { defenderHP, HpD},
        { offenderPos, PosO}, {defenderPos, PosD},
        { offenderPosAct, OffPosAct}, {defenderPosAct, DefPosAct}
    ]}.

trans(S,
       #{range_type:=RangeTypeO,
         curr_hand:={HandType, AttackType, DamageRange}, prim_hand:=PrimHand, secd_hand:=SecdHand,
         attr:=#{damage_multiplier:=DamageMul, critical_multiplier:=CritMul, damage_addon:=DamageAddon},
         state:=#{rem_moves:=RemMoves, position:=PosO}=StateA}=A,
       #{attr:=#{armor:=Armor}=CurrAttrD, state:=#{hp:=H2, position:=PosD}=StateD}=D, L) ->

    Outcome = roulette(A, D),
    
    Damage = calculate_damage(AttackType, Outcome, DamageRange, Armor),

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
    #{state:=NextStateD} = NextD,


    {NewPosO, NewPosD, NewPosActO, NewPosActD} = case is_no_damage_move(A) of
        
        true -> {PosO, PosD, stand, stand};
        
        _    -> case binary_to_atom(RangeTypeO, utf8) of
        
            near when (PosO + PosD) < 5 ->
                {5 - PosD, PosD, chase, not_assigned_yet};

            near ->
                {PosO, PosD, stand, not_assigned_yet};

            far when (PosO + PosD) < 3 ->
                {PosO + 1, PosD, chase, not_assigned_yet};

            far when PosO + PosD == 5 ->
                case PosO of
                    1 -> {PosO, PosD - 2, stand, back_jump_2};
                    _ -> {PosO - 1, PosD, back_jump, not_assigned_yet}
                end;

            far ->
                {PosO, PosD, stand, not_assigned_yet}
        
        end
    end,

    {NewPosDAfter, NewPosActDAfter} = case {NewPosD, NewPosActD} of
        {1, not_assigned_yet} -> {1, stand};
        {_, not_assigned_yet} ->
            case rand:uniform() > 0.5 of
                true -> {NewPosD - 1, blown_out};
                _ -> {NewPosD, stand}
            end;
        _ -> {NewPosD, NewPosActD}
    end,

    erlang:display({NewPosActO, NewPosActDAfter, NewPosO, NewPosDAfter}),

    NextLog = case is_no_damage_move(A) of
        true -> L;
        _ -> [log(S, A, NextD, NewPosActO, NewPosActDAfter, NewPosO, NewPosDAfter) | L]
    end,
   
    {NextA#{state:=StateA#{rem_moves:=RemMoves-1, position:= NewPosO}}, NextD#{state:=NextStateD#{position:=NewPosDAfter}}, NextLog}.


apply(State, #{attr:=#{attack_disabled:=0}}=O, D, Log) ->

    {#{state:=#{rem_moves:=RemMovesO}}=MovedO, MovedD, MovedLog} = trans(State, O, D, Log),

    DoneMovedO = case RemMovesO of
        0 -> MovedO#{done:=already};
        _ -> MovedO
    end,

    {DefReactedD, DefReactedO, DefReactedLog} = effect:apply(State, MovedD, DoneMovedO, MovedLog),
    {ReactedO, ReactedD, ReactedLog} = effect:apply(State, DefReactedO, DefReactedD, DefReactedLog),

    {ReactedO, ReactedD, ReactedLog};

apply(_State, #{state:=StateO}=O, D, Log) ->
    {O#{done:=already, state:=StateO#{rem_moves:=0}}, D, Log}.
