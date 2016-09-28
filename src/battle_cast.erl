-module(battle_cast).

-author('Yue Marvin Tao').


% A effects entry in a list should conform to the format of:

% Modify attribute and return new player profile
get_attr(AttrName, P) ->
    #{curr_attr:=#{AttrName:=Value}} = P, Value.

set_attr(AttrName, P, Value) ->
    P#{curr_attr:=#{AttrName=>Value}}.


make_effect({direct, Value, {ToWhat, ToWhom, AttrName}}, {#{id:=I1}=P1, #{id:=I2}=P2}) ->
    case {ToWhat, ToWhom} of
        {to_hp, I1} -> {P1#{hp:=Value}, P2};
        {to_hp, I2} -> {P1, P2#{hp:=Value}};
        {to_attr, I1} -> {set_attr(AttrName, P1, Value), P2};
        {to_attr, I2} -> {P1, set_attr(AttrName, P2, Value)}
    end;


make_effect({indirect, Ratio, {FromWhat, FromWhom, AttrName}, To},
            {#{id:=I1, hp:=H1}=P1, #{id:=I2, hp:=H2}=P2}) ->
    case {FromWhat, FromWhom} of
        {from_hp, I1} -> make_effect({direct, H1 * Ratio, To}, {P1, P2});
        {from_hp, I2} -> make_effect({direct, H2 * Ratio, To}, {P1, P2});
        {from_attr, I1} -> make_effect({direct, get_attr(AttrName, P1) * Ratio, To}, {P1, P2});
        {from_attr, I2} -> make_effect({direct, get_attr(AttrName, P1) * Ratio, To}, {P1, P2})
    end.


make_effect(Effect, State, {#{id:=I1}=P1, P2}) ->

    {_Name, {Seq, Phase, Outcome}, Specs} = Effect,

    {CurrSeq, CurrPhase, _, {Mover, _, _}, _} = State,

    OutcomeMatches = case Outcome of
        nah -> true;
        _   -> case Mover of
            I1 -> Outcome == get_attr(outcome, P1);
            _  -> Outcome == get_attr(outcome, P2)
        end
    end,
    
    case (Seq > CurrSeq) and (Phase == CurrPhase) and (OutcomeMatches == true) of
        true -> make_effect(Specs, {P1, P2});
        _    -> {P1, P2}
    end.

apply_effects({_, _, _, _, []}, P1, P2) -> {P1, P2};
apply_effects(S = {_, _, _, _, [EffectDescription | Remaining]}, P1, P2) ->
    
    erlang:display(EffectDescription),
    
    {AffectedP1, AffectedP2} = make_effect(EffectDescription, S, {P1, P2}),
    
    apply_effects(setelement(5, S, Remaining), AffectedP1, AffectedP2).


set_whom(to_self, Mover, I1, I2) -> Mover;
set_whom(to_opponent, Mover, I1, I2) when Mover == I1 -> I2;
set_whom(to_opponent, _, I1, _) -> I1.

cast(S, #{id:=I1, cast_list:=Cast1}=P1, #{id:=I2, cast_list:=Cast2}=P2) ->

    {CurrSeq, CurrPhase, DefOff, M={Mover, _, _}, EffectList} = S,

    {Cast, NewCast1, NewCast2} = case Mover of
        I1 -> {hd(Cast1), tl(Cast1), Cast2};
        _  -> {hd(Cast2), Cast1, tl(Cast2)}
    end,

    {Name, Cond, Specs} = Cast,

    {Last, Phase, Outcome} = Cond,
    NewCond = {Last + CurrSeq, Phase, Outcome},

    NewSpecs = case Specs of
        {direct, V, {ToWhat, ToWhom, Attr}} ->
            {direct, V, {ToWhat, set_whom(ToWhom, Mover, I1, I2), Attr}};
        {indirect, V, {FromWhat, FromWhom, FromAttr}, {ToWhat, ToWhom, ToAttr}} ->
            {indirect, V,
                {FromWhat, set_whom(FromWhom, Mover, I1, I2), FromAttr},
                {ToWhat, set_whom(ToWhom, Mover, I1, I2), ToAttr}
            }
    end,

    NewEffectList = [{Name, NewCond, NewSpecs} | EffectList],

    NewS = {CurrSeq, CurrPhase, DefOff, M, NewEffectList},

    {NewS, P1#{cast_list:=NewCast1}, P2#{cast_list:=NewCast2}}.

create_cast_table() ->

    AvailableCasts = [
        {assault, {1, casting, nah}, {value, 100, {to_hp, to_opponent, null}}}
    ].



