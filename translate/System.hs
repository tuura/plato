-----------------------------------------------------------------------------
-- |
-- Module     : Translation/System.hs
-- Copyright  : (c) 2015-2018, Tuura authors
-- License    : BSD (see the file LICENSE)
-- Maintainer : jonathan.r.beaumont@gmail.com
-- Stability  : experimental
--
-- Plato is a tool which embeds the Asynchronous Concepts language in Haskell.
-- This language is used for the specification of asynchronous circuits, and
-- is fully compositional and highly reusable, from individual concepts to
-- entire concepts specifications.

-- Plato can also compile and validate Asynchronous Concepts, with the help of
-- the GHC. Compiled concepts can then be translated to existing modelling
-- formalisms of Signal Transition Graphs (STGs) and State Graphs. These models
-- feature a long history of theory and therefore several tools which can be
-- used for verification and synthesis. STGs and State Graphs can be visualized
-- in Workcraft (https://workcraft.org), where Plato and the tools for these
-- models are all integrated.
--
-- This module is used in the compilation of a 'System' concept specification.
-- A 'System' concept is when the signals included in the system are defined
-- as a data type. The module imports the 'System' concept in question, and
-- the options from calling the tool. This is then compiled, producing a
-- binary which when run will output the STG in .g format.
--
-----------------------------------------------------------------------------

module Main (main) where

-- | FSM and STG translation modules are imported, as a 'System' concept can be
-- translated to either of these modules. The 'Translation' module is also
-- imported to prepare the concepts and validate them before translation can
-- occur.
import qualified Tuura.Concept.FSM as FSM hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.FSM.Translation as FSM

import qualified Tuura.Concept.STG as STG hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.STG.Translation as STG

import qualified Tuura.Plato.Translate.Translation as T
import Tuura.Plato.Translate.Options

-- | Import the concept to be translated.
import Concept

-- | This starts by getting the options, then converts the concept
-- specification to use the 'Signal' type instead of the polymorphic @a@ type,
-- and it sets the Signals up to be used by the translation. It will then
-- translate the specification to an FSM or an STG, depending on the options
-- provided when the tool is called. If the validation passes, the STG or
-- FSM will be output, otherwise, it will output the errors.
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
