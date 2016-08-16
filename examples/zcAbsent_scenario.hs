module Concept where

import Tuura.ConceptConcat

--ZC absent scenario definition using concepts
circuit :: Eq a => a -> a -> a -> a -> a -> a -> a -> CircuitConcept a
circuit uv oc zc gp_ack gn_ack gp gn =
    chargeFunc <> uvFunc <> uvReact -- <> zcAbsent
  where
    uvFunc = rise uv ~> rise gp <> rise uv ~> fall gn
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    uvReact = rise gp_ack ~> fall uv <> fall gn_ack ~> fall uv
    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc <> fall gn_ack ~> rise gp <> fall gp_ack ~> rise gn
    circuitConstraint     = me gp gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise uv False <> initialise oc False

    chargeFunc = ocFunc <> ocReact <> environmentConstraint <> circuitConstraint 
                 <> gpHandshake <> gnHandshake <> initialState

--    zcAbsent = silent zc
