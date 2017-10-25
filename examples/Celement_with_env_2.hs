module Concept where

import Tuura.Concept.STG

-- C-element with environment circuit described using gate-level concepts
component a b c = interface <> cElement a b c <> environment <> initialState
  where
    interface = inputs [a, b] <> outputs [c]

    environment = inverter c a <> inverter c b

    initialState = initialise a False <> initialise b False <> initialise c False



