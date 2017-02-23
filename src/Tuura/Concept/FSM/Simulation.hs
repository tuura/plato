module Tuura.Concept.FSM.Simulation (
    module Control.Monad.State,
    Simulation, PureSimulation, runSimulationFSM, runPureSimulationFSM
    ) where

import Tuura.Concept.Circuit.Derived
import Control.Monad.State hiding (State)
import Data.Functor.Identity

type Simulation a m = StateT (State a) m

type PureSimulation a = Simulation a Identity

runSimulationFSM :: Simulation a m r -> State a -> m (r, State a)
runSimulationFSM = runStateT

runPureSimulationFSM :: PureSimulation a r -> State a -> (r, State a)
runPureSimulationFSM = runState
