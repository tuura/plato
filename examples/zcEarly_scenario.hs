module Concept where

import Tuura.ConceptConcat

--ZC early scenario definition using concepts
circuit :: Eq a => a -> a -> a -> a -> a -> a -> a -> CircuitConcept a
circuit uv oc zc gp_ack gn_ack gp gn =
    chargeFunc <> zcFunc <> zcReact <> uvFunc' <> uvReact' <> initialise zc False
  where
    zcFunc = rise zc ~> fall gn 
    zcReact = fall oc ~> rise zc <> rise gp ~> fall zc

    uvFunc' = rise uv ~> rise gp
    uvReact' = rise zc ~> rise uv <> fall zc ~> fall uv <> rise gp_ack ~> fall uv

--Must be redefined as auto re-use of concepts is yet to be implemented
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc <> fall gn_ack ~> rise gp <> fall gp_ack ~> rise gn
    circuitConstraint     = me gp gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise uv False <> initialise oc False

    chargeFunc = ocFunc <> ocReact <> environmentConstraint <> circuitConstraint 
                 <> gpHandshake <> gnHandshake <> initialState
