module Concept where

import Tuura.ConceptConcat

-- C-element with environment circuit described using gate-level concepts
circuit :: (Eq a) => a -> a -> a -> CircuitConcept a
circuit a b c = cElement a b c <> environment <> initialState
  where
    environment = inverter c a <> inverter c b

    initialState = initialise a False <> initialise b False <> initialise c False
