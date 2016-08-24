module Concept where

import Tuura.ConceptConcat

-- C-element with environment circuit described using gate-level concepts
circuit :: (Eq a) => a -> a -> a -> CircuitConcept a
circuit a b c = interface <> cElement a b c <> environment <> initialState
  where
    interface = inputs [a, b] <> outputs [c]

    environment = inverter c a <> inverter c b

    initialState = initialise a False <> initialise b False <> initialise c False



