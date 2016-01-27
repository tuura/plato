module Circuit.Dynamics where

import Circuit
import Circuit.Concept

allTransitions :: (Enum a, Bounded a) => [Transition a]
allTransitions =
    [Transition a b | a <- [minBound .. maxBound], b <- [False, True]]

enabledTransitions :: (Enum a, Bounded a) => State a -> CircuitConcept a -> [Transition a]
enabledTransitions s c = filter (\t -> excited c t s) allTransitions

fire :: Eq a => Transition a -> State a -> State a
fire t (State value) =
    State $ \a -> if a == signal t then newValue t else value a

-- initialStates :: CircuitConcept a -> [State a]
-- initialStates =

-- reachableStates :: CircuitConcept a -> [State a]
-- reachableStates =
