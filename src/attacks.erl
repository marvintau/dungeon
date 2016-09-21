-module(attacks).
-author('Yue Marvin Tao').

-export([plain_attack/1]).

rotate(Roulette) ->

    Cumulative = lists:foldl(fun(X, Rem) -> [X + hd(Rem) | Rem] end, [0], Roulette),

    Rand = rand:uniform() * hd(Cumulative),
    
    ResultIndex = length(element(1, lists:splitwith(fun(X) -> X > Rand end, Cumulative))),
    lists:nth(ResultIndex, [block, resist, dodge, critic, attack]).

% Preparing the roulette, might be affected by the buff or other conditions
% specified in Battle Context
prepare_roulette_from(
    #{curr_hand:={_, PrimType, _}, resist:=Res, hit:=Hit, critic:=Critic},
    #{secd_type:=Secd, block:=Blo, dodge:=Dod}, _B
) ->

    {Dodge, Resist, Block} = case {PrimType, Secd} of
        
        {{_, magic}, _} ->
            {0, Res, 0};
        
        {{_, physical}, {_, shield}} ->
            {Dod, 0, Blo};

        {{_, physical}, _} ->
            {Dod, 0, 0};

        % For the condition that supposed not to happen. should be avoided
        % before rotating the roulette.
        {_, _} ->
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

single_attack(#{curr_hand:=CurrHand}, #{armor:=Armor}, Outcome) ->

    Random = rand:uniform(),

    case CurrHand of

        {_, {_, magic}, AtkRange} ->
            magic_damage(Random, Outcome, AtkRange);
        {_, {_, physical}, AtkRange} ->
            physical_damage(Random, Outcome, Armor, AtkRange);
        _ ->
            0
    end.


plain_attack({
    #{curr_hand:={_, {no_damage, _}, _}, prim_range:=Range, prim_type:=Type}=A, D,
    #{rem_atk:=RemAtk}=B
}) ->
    
    { A#{curr_hand := {prim, Type, Range}}, D, B#{rem_atk:= RemAtk - 1} };


plain_attack({
    #{curr_hand:=CH, prim_type:=PT, prim_range:=PR, secd_type:=ST, secd_range:=SR, damage_coeff:=DC}=A,
    D, #{rem_atk:=RemAtk}=B
 }) ->

    Outcome = rotate(prepare_roulette_from(A, D, B)),

    NewBattle = B#{
        outcome => Outcome,
        rem_atk => RemAtk - 1,
        def_damage => single_attack(A, D, Outcome) * DC
    },

    NewAttack = A#{
        curr_hand := case CH of
            {prim, _, _} ->
                {secd, ST, SR};
            _ ->
                {prim, PT, PR}
            end
    },
       
    NewDefense = D#{
        hp := maps:get(hp, D) - maps:get(def_damage, NewBattle)
    },

    % NextLog always records the Attacker's context before updated, and
    % Defenser's context after updated.

    {NewAttack, NewDefense, NewBattle}.
