-module(battle_attack).
-author('Yue Marvin Tao').

-export([get_final_damage/2]).

% ------------------------ ROTATE ROULETTE ----------------------------
% get the random choice result according to the probability of each 
% option.

rotate(Roulette) ->

    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = rand:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, Cumulative))),
    lists:nth(ResultIndex, [block, resist, dodge, critic, attack]).


% ----------------------- PREPARE ROULETTE ----------------------------
% roulette is generated from the attributes of both offender and defenser.
% It may varied due to the different weapon in hand. The rule of getting
%

prepare_roulette_from(
    #{curr_hand:={_, Curr, _}, curr_attr:=#{resist:=Res, hit:=Hit, critic:=Critic}},
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

magic_damage(Random, critic, {Lower, Upper}) ->
    round((Lower + Random * (Upper - Lower)) * 2);

magic_damage(Random, _, {Lower, Upper}) ->
    round(Lower + Random * (Upper - Lower)).


% --------------- PLAYER AS NON-MAGE --------------------------
% Physical attack cannot be resisted, make sure that resist has
% been ser 0 before turning the roulette.

physical_damage(_, dodge, _, _) -> 0;
physical_damage(_, block, _, _) -> 0;
physical_damage(Random, critic, Armor, {Lower, Upper}) ->
    round((Lower + Random * (Upper - Lower)) * 2 * (1 - Armor * 0.0001));

physical_damage(Random, _, Armor, {Lower, Upper}) ->
    round(Lower + Random * (Upper - Lower) * (1 - Armor * 0.0001)).


% ============= SINGLE ATTACK DAMAGE CALCULATION ======================

% Calculates the damage with given character type, the upper and lower
% damage of weapon, and outcome of roulette turning.

single_attack(#{curr_hand:=CurrHand}, #{curr_attr:=#{armor:=Armor}}, Outcome) ->

    Random = rand:uniform(),

    case CurrHand of

        {_, {_, magic}, AtkRange} ->
            magic_damage(Random, Outcome, AtkRange);
        {_, {_, physical}, AtkRange} ->
            physical_damage(Random, Outcome, Armor, AtkRange);
        _ ->
            0
    end.


get_final_damage(#{damage_coeff:=DC}=A, D) ->

    Outcome = rotate(prepare_roulette_from(A, D)),

    Damage  = single_attack(A, D, Outcome) * DC, 

    {Outcome, Damage}.
