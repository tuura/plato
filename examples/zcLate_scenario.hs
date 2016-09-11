module BuckConverter where

import Tuura.Concept.STG

--ZC late scenario definition using concepts
circuit uv oc zc gp_ack gn_ack gp gn =
    chargeFunc <> uvFunc <> uvReact <> zcLate <> initialise zc False
  where
    interface = inputs [uv, oc, zc, gp_ack, gn_ack] <> outputs [gp, gn]

    zcLate = rise uv ~> rise zc <> fall zc ~> fall uv

--Must be redefined as auto re-use of concepts is yet to be implemented
    uvFunc = rise uv ~> rise gp <> rise uv ~> fall gn
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    uvReact = rise gp_ack ~> fall uv <> fall gn_ack ~> fall uv
    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc <> fall gn_ack ~> rise gp <> fall gp_ack ~> rise gn
    circuitConstraint     = me gp gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise0 [uv, oc, zc, gp_ack, gn_ack, gp, gn]

    chargeFunc = interface <> ocFunc <> ocReact <> environmentConstraint
                <> circuitConstraint <> gpHandshake <> gnHandshake <> initialState
