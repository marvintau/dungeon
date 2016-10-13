-module(battle_attack).
-author('Yue Marvin Tao').

-export([attack/4]).

% ------------------------ ROTATE ROULETTE ----------------------------
% get the random choice result according to the probability of each 
% option.

rotate(Roulette) ->

    % reversed
    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = rand:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, Cumulative))),
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
        
        {{_, magic}, _} ->
            {0, Res, 0};
        
        {{_, physical}, {_, shield}} ->
            {Dod, 0, Blo};

        {{_, physical}, _} ->
            {Dod, 0, 0};
        _ ->             
            {0, 0, 0}            
    end,

    [Hit, Critic, Dodge, Resist, Block].

% --------------- PLAYER AS MAGE ------------------------------
% Magic attack cannot be blocked, thus make sure that block has
% been set 0 before turning the roulette.

magic_damage(Random, resist, {Lower, _}) -> round(Random * Lower / 10);

magic_damage(Random, critical, {Lower, Upper}) ->
    round(2*(Lower + Random * (Upper - Lower)));

magic_damage(Random, _, {Lower, Upper}) ->
    round(Lower + Random * (Upper - Lower)).


% --------------- PLAYER AS NON-MAGE --------------------------
% Physical attack cannot be resisted, make sure that resist has
% been ser 0 before turning the roulette.

physical_damage(_, dodge, _, _) -> 0;
physical_damage(_, block, _, _) -> 0;
physical_damage(Random, critical, Armor, {Lower, Upper}) ->
    round(2*(Lower + Random * (Upper - Lower)) * (1 - Armor * 0.0001));

physical_damage(Random, _, Armor, {Lower, Upper}) ->
    round(Lower + Random * (Upper - Lower) * (1 - Armor * 0.0001)).


% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

% Calculates the damage with given character type, the upper and lower
% damage of weapon, and outcome of roulette turning.

damage(#{curr_hand:=CurrHand, damage_coeff:=Coeff}=A, #{curr_attr:=#{armor:=Armor}}=D) ->

    Outcome = rotate(prepare_roulette_from(A, D)),
    
    Damage = case CurrHand of

        {_, {_, magic}, DamageRange} ->
            magic_damage(rand:uniform(), Outcome, DamageRange);
        {_, {_, physical}, DamageRange} ->
            physical_damage(rand:uniform(), Outcome, Armor, DamageRange);
        _ ->
            0
    end,

    {attack, Outcome, Damage * Coeff}.


log({Seq, Stage, Role, {Mover, _, Rem}, _},
           #{curr_attr:=#{outcome:=Outcome, damage_dealt:=Damage}=_, 
             curr_hand:={Which, {_, AtkType}, _}}=O, D)  ->
    
    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover }, {role, Role}, { defender, maps:get(id, D)},

        { hand, Which}, { action, AtkType}, {rem_atks, Rem},
        { outcome, Outcome }, { damage, Damage },
        { offender_hp, maps:get(hp, O) },
        { defender_hp, maps:get(hp, D) }
    ]}.

attack(S, #{curr_attr:=CurrAttr}=A, #{hp:=H2}=D) ->
    {attack, Outcome, Damage} = damage(A, D),
    NextA = A#{curr_attr:=CurrAttr#{damage_dealt:=Damage, outcome:=Outcome}},
    NextD = D#{hp:=H2 - Damage},
    NextLog = log(S, NextA, NextD),
    {NextA, NextD, NextLog}.

attack(
  S = {_, _, _, {Mover, Hand, RemainingMoves}, _},
  #{id:=I1, curr_hand:=Curr1, prim_hand:=Prim1, secd_hand:=Secd1} = P1,
  #{id:=I2, curr_hand:=Curr2, prim_hand:=Prim2, secd_hand:=Secd2} = P2, L) ->

    {NextP1, NextP2, NextLog} = case Mover of
        I1 -> attack(S, P1, P2);
        _ ->  {New2, New1, NewLog} = attack(S, P2, P1), {New1, New2, NewLog}
    end,

    {NewCurr1, NewCurr2, NewHand} = case {Mover, Hand} of
        {I1, prim} -> {Secd1, Curr2, secd};
        {I1, secd} -> {Prim1, Curr2, prim};
        {I2, prim} -> {Curr1, Secd2, secd};
        {I2, secd} -> {Curr1, Prim2, prim}
    end,

    { setelement(4, S, {Mover, NewHand, RemainingMoves-1}),
      NextP1#{curr_hand:=NewCurr1},
      NextP2#{curr_hand:=NewCurr2},
      [ NextLog | L ]
    }. 
