module Tuura.Concept.Concat.Simulation (
    module Control.Monad.State,
    Simulation, PureSimulation, runSimulation, runPureSimulation
    ) where

import Tuura.Concept.Concat.Circuit
import Control.Monad.State hiding (State)
import Data.Functor.Identity

type Simulation a m = StateT (State a) m

type PureSimulation a = Simulation a Identity

runSimulation :: Simulation a m r -> State a -> m (r, State a)
runSimulation = runStateT

runPureSimulation :: PureSimulation a r -> State a -> (r, State a)
runPureSimulation = runState
