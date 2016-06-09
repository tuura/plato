module Tuura.Concept.Simulation (
    module Control.Monad.State,
    Simulation, PureSimulation, runSimulation, runPureSimulation,
    enabledTransitions, enabled, fire
    ) where

import Tuura.Concept.Abstract
import Tuura.Concept.Circuit
import Control.Monad.State hiding (State)
import Data.Functor.Identity

type Simulation a m = StateT (State a) m

type PureSimulation a = Simulation a Identity

runSimulation :: Simulation a m r -> State a -> m (r, State a)
runSimulation = runStateT

runPureSimulation :: PureSimulation a r -> State a -> (r, State a)
runPureSimulation = runState

allTransitions :: (Enum a, Bounded a) => [Transition a]
allTransitions =
    [Transition a b | a <- [minBound .. maxBound], b <- [False, True]]

enabledTransitions :: (Enum a, Bounded a, Monad m) => CircuitConcept a -> Simulation a m [Transition a]
enabledTransitions c = do
    s <- get
    return $ filter (\t -> excited c t s) allTransitions

enabled :: Monad m => CircuitConcept a -> Transition a -> Simulation a m Bool
enabled c t = get >>= return . excited c t

fire :: (Eq a, Monad m) => Transition a -> Simulation a m ()
fire t = do
    State value <- get
    put . State $ \a -> if a == signal t then newValue t else value a

-- initialStates :: CircuitConcept a -> [State a]
-- initialStates =

-- reachableStates :: CircuitConcept a -> [State a]
-- reachableStates =
