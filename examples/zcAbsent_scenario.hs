module BuckConverter where

import Tuura.Concept.STG

--ZC absent scenario definition using concepts
circuit uv oc zc gp gp_ack gn gn_ack =
    chargeFunc <> uvFunc <> uvReact -- <> zcAbsent
  where
    interface = inputs [uv, oc, zc, gp_ack, gn_ack] <> outputs [gp, gn]

    uvFunc = rise uv ~> rise gp <> rise uv ~> fall gn
    ocFunc = rise oc ~> fall gp <> rise oc ~> rise gn

    uvReact = rise gp_ack ~> fall uv <> fall gn_ack ~> fall uv
    ocReact = fall gp_ack ~> fall oc <> rise gn_ack ~> fall oc

    environmentConstraint = me uv oc
    noShortcircuit        = me gp gn <> fall gn_ack ~> rise gp <> fall gp_ack ~> rise gn

    gpHandshake = handshake gp gp_ack
    gnHandshake = handshake gn gn_ack

    initialState = initialise0 [uv, oc, zc, gp, gp_ack] <> initialise1 [gn, gn_ack]

    chargeFunc = interface <> ocFunc <> ocReact <> environmentConstraint <>
                 noShortcircuit <> gpHandshake <> gnHandshake <> initialState

--    zcAbsent = silent zc
