-module(battle_attack).
-author('Yue Marvin Tao').

-export([attack/4]).

% ------------------------ ROTATE ROULETTE ----------------------------
% get the random choice result according to the probability of each 
% option.

rotate(Roulette) ->

    erlang:display(Roulette),
    % reversed
    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = rand:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X >= Rand end, Cumulative))),
    lists:nth(ResultIndex, [block, resist, dodge, critical, attack]).


% ----------------------- PREPARE ROULETTE ----------------------------
% Roulette is generated from the player's attribute, the current weapon
% of current player in move, and the secondary weapon of the defender.
% Magic attack is not dodgable and blockable, Physical attack might be
% blocked if the defender is carrying a shield, or can be only dodged
% otherwise.

prepare_roulette_from(
    #{curr_hand:={_, Curr, _}, curr_attr:=#{resist:=Res, hit:=Hit, critical:=Critic}},
    #{secd_hand:={_, Secd, _}, curr_attr:=#{block:=Blo, dodge:=Dod}}
) ->

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

    [Hit, Critic, Dodge, Resist, Block].

% --------------- PLAYER AS MAGE ------------------------------
% Magic attack cannot be blocked, thus make sure that block has
% been set 0 before turning the roulette.

calculate_damage(magic, resist, {Lower, _}, _Armor) ->
    round(rand:uniform() * Lower / 10);

calculate_damage(magic, critical, {Lower, Upper}, _Armor) ->
    round(2*(Lower + rand:uniform() * (Upper - Lower)));

calculate_damage(magic, _, {Lower, Upper}, _Armor) ->
    round(Lower + rand:uniform() * (Upper - Lower));

calculate_damage(physical, dodge, _, _) -> 0;
calculate_damage(physical, block, _, _) -> 0;
calculate_damage(physical, critical, {Lower, Upper}, Armor) ->
    round(2*(Lower + rand:uniform() * (Upper - Lower)) * (1 - Armor * 0.0001));

calculate_damage(physical, _, {Lower, Upper}, Armor) ->
    round(Lower + rand:uniform() * (Upper - Lower) * (1 - Armor * 0.0001));

calculate_damage(_, _, _, _) -> 0.

% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

% Calculates the damage with given character type, the upper and lower
% damage of weapon, and outcome of roulette turning.

log({Seq, Stage, Mover},
    #{curr_hand:={Which, WeaponType, _}}=O,
    #{curr_attr:=#{outcome:=Outcome, damage_taken:=Damage}}=D)  ->
    
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, { defender, maps:get(id, D)},
        { hand, Which}, { action, WeaponType},
        { outcome, Outcome }, { damage, Damage },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.

attack(S,
       #{curr_hand:={HandType, AttackType, DamageRange}, prim_hand:=PrimHand, secd_hand:=SecdHand,
         curr_attr:=#{damage_coeff:=DamageCoeff, damage_addon:=DamageAddon}=CurrAttr, rem_moves:=RemMoves}=A,
       #{curr_attr:=#{armor:=Armor}, hp:=H2}=D, L) ->

    Outcome = rotate(prepare_roulette_from(A, D)),
    
    Damage = calculate_damage(AttackType, Outcome, DamageRange, Armor) * DamageCoeff + DamageAddon,

    NextA = case HandType of
        prim -> A#{curr_hand:=SecdHand};
        secd -> A#{curr_hand:=PrimHand}
    end,
    NextD = D#{curr_attr:=CurrAttr#{damage_taken:=Damage, outcome:=Outcome}, hp:=H2 - Damage},
    NextLog = [log(S, A, NextD) | L],
    
    {NextA#{rem_moves:=RemMoves-1}, NextD, NextLog}.
