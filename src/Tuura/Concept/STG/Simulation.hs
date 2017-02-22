module Tuura.Concept.STG.Simulation (
    module Control.Monad.State,
    Simulation, PureSimulation, runSimulationSTG, runPureSimulationSTG
    ) where

import Tuura.Concept.Circuit.Derived
import Control.Monad.State hiding (State)
import Data.Functor.Identity

type Simulation a m = StateT (State a) m

type PureSimulation a = Simulation a Identity

runSimulationSTG :: Simulation a m r -> State a -> m (r, State a)
runSimulationSTG = runStateT

runPureSimulationSTG :: PureSimulation a r -> State a -> (r, State a)
runPureSimulationSTG = runState
