module BuckConverter where

import Tuura.Concept.STG

--ZC early scenario definition using concepts
circuit uv oc zc gp gp_ack gn gn_ack =
    chargeFunc <> zcFunc <> zcReact <> uvFunc' <> uvReact' <> initialise zc False
  where
    interface = inputs [uv, oc, zc, gp_ack, gn_ack] <> outputs [gp, gn]

    zcFunc = rise zc ~> fall gn
    zcReact = fall oc ~> rise zc <> rise gp ~> fall zc

    uvFunc' = rise uv ~> rise gp
    uvReact' = rise zc ~> rise uv <> fall zc ~> fall uv <> rise gp_ack ~> fall uv

--Must be redefined as auto re-use of concepts is yet to be implemented
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc
    noShortcircuit        = me gp gn <> fall gn_ack ~> rise gp <> fall gp_ack ~> rise gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise0 [uv, oc, zc, gp, gp_ack] <> initialise1 [gn, gn_ack]

    chargeFunc = interface <> ocFunc <> ocReact <> environmentConstraint <>
                 noShortcircuit <> gpHandshake <> gnHandshake <> initialState
