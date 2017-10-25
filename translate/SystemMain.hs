module Main (main) where

import qualified Tuura.Concept.FSM as FSM hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.FSM.Translation as FSM

import qualified Tuura.Concept.STG as STG hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.STG.Translation as STG

import Tuura.Plato.Translate.Translation
import Tuura.Plato.Translate.Options

-- Import the concept to be translated.
import Concept

main :: IO ()
main = do
    options <- getOptions
    let output = optOutput options
        c = convert circuit
        numSigns = fromEnum (maxBound :: Sign)
        signals = [Signal i | i <- [0..numSigns]]
        result = if (optFSM options)
            then FSM.translate c signals
            else STG.translate c signals
    if fst result
      then output (snd result)
      else putStrLn (snd result)
