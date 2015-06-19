module BuckExample (
	zcAbsentScenario
    ) where

import Circuit
import Circuit.Concept

zcAbsentScenario :: Eq a => a -> a -> a -> a -> a -> a -> a -> CircuitConcept a
zcAbsentScenario uv oc zc gp gp_ack gn gn_ack = chargeFunc <> uvFunc <> uvReact <> zcAbsent
  where
  	uvFunc = rise uv ~> rise gp <> rise uv ~> fall gn
  	ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

  	uvReact = rise gp_ack ~> fall uv <> fall gn_ack ~> fall uv
  	ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

  	environmentConstraint = me uv oc
  	circuitConstraint     = me gp gn

  	gpHandshake = handshake gp gp_ack
  	gnHandshake = handshake gn gn_ack

  	initialState = initialConcept $ before (rise uv) .&&. before (rise oc)

	chargeFunc = ocFunc <> ocReact <> environmentConstraint <> circuitConstraint <> gpHandshake <> gnHandshake

	zcAbsent = silent (rise zc) <> silent (fall zc)
