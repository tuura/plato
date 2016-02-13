module Tuura.Concept.Examples.SimpleBuck (
    zcAbsentScenario
    ) where

import Data.Monoid
import Tuura.Concept

zcAbsentScenario :: Eq a => a -> a -> a -> a -> a -> a -> a -> CircuitConcept a
zcAbsentScenario uv oc zc gp gp_ack gn gn_ack =
    chargeFunc <> uvFunc <> uvReact <> zcAbsent <> initialState
  where
    uvFunc = rise uv ~> rise gp <> rise uv ~> fall gn
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    uvReact = rise gp_ack ~> fall uv <> fall gn_ack ~> fall uv
    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc
    circuitConstraint     = me gp gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise uv False <> initialise oc False

    chargeFunc = ocFunc <> ocReact <> environmentConstraint <> circuitConstraint <> gpHandshake <> gnHandshake

    zcAbsent = silent zc
