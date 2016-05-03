module Concept where

import Tuura.Concept

-- Signals
data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

-- Example circuit described using gate-level concepts
circuit :: (Eq a) => a -> a -> a -> CircuitConcept a
circuit a b c = consistency <> cElement a b c <> inverter c a <> inverter c b
