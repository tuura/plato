module Main (main) where

import qualified Tuura.Concept.FSM as FSM hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.FSM.Translation as FSM

import qualified Tuura.Concept.STG as STG hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.STG.Translation as STG

import qualified Tuura.Plato.Translate.Translation as T
import Tuura.Plato.Translate.Options

-- Import the concept to be translated.
import Concept

main :: IO ()
main = do
    options <- getOptions
    let output = optOutput options
        c = T.convert system
        numSigns = fromEnum (maxBound :: Signal)
        signals = [T.Signal i | i <- [0..numSigns]]
        result = if (optFSM options)
            then FSM.translate c signals
            else STG.translate c signals
    if fst result
      then output (snd result)
      else putStrLn (snd result)
