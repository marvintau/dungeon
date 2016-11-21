# STATE MACHINE DESIGN
the FSM begins with prepare. The state transition condition will be same
as the last state before toss function.

TOSS going to SETTLE, SETTLE going to SWAP, POST_CAST going to SWAP are 
directly and determined, and would be controlled by a general dispatcher.

SWAP checks whether both the players have finished this stage. Proceed if
both are done by swapping, or just swap in current stage. Thus, only the
transition from SETTLE is the unconditional.

The transitions:

SETTLE: if both done -> swap & go CAST 
        if not both done -> swap

CAST     :  -> go POSTCAST
POSTCAST :  if both done -> swap & go ATTACK
            if not both done -> swap & go CAST

ATTACK   :  -> go POSTATTACK
POSTATTACK : if not both done -> swap
             if both done -> if single ATTACK not done -> go ATTACK
                             if single ATTACK done -> swap and go ATTACK

Problem : carrying out effects of two players will be very complex because
          we have to temporarily store the current attacking player before
          carrying out post-attack effects of both players.

Solution:

1. move the effect lists from both players to the state structure

   a. when interpreting the description of effect, replace the offender
      and defender with the actual player id

   b. when carrying out the effect, load the player context directly from
      the id.

2. create a new stage dedicated to carrying out effects, instead of carrying
   out effects right after casting or attacking within one single stage.


