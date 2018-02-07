-----------------------------------------------------------------------------
-- |
-- Module     : Translation/Component.hs
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
-- This module is used in the compilation of a 'Component' concept
-- specification. A 'Component' concept is when the signals used in the
-- component are included as parameters in the concept. The component concept
-- is validated, compiled using 'Hint', a Haskell Runtime Interpreter, and then
-- translated to the chosen model type, STG or FSM.
--
-----------------------------------------------------------------------------

module Main (main) where

import Data.List
import Data.List.Extra
import Data.Char
import Text.Printf
import qualified Data.Text as Text
import System.Directory
import Control.Exception
import System.Environment
import System.IO.Error

-- | Both STG and FSM translation modules are imported, as either can be
-- the target of translation regardless of the concept specification used.
import qualified Tuura.Concept.FSM as FSM hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.FSM.Simulation as FSM
import qualified Tuura.Concept.FSM.Translation as FSM

import qualified Tuura.Concept.STG as STG hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.STG.Simulation as STG
import qualified Tuura.Concept.STG.Translation as STG

import Tuura.Plato.Translate.Options

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

-- | Takes the options from calling the tool, finds the input concept file,
-- the included files, and performs the validation and translation on these.
main :: IO ()
main = do
    options <- getOptions
    let input = optInput options
    let paths = [input] ++ optInclude options
    r <- GHC.runInterpreter $ doWork (optFSM options) paths (optOutput options)
    either (putStrLn . displayException) return r

-- | Our own Signal type. Contains the signal index, from 0 to x-1 if
-- there are x signals.
data Signal = Signal Int deriving Eq

-- | The 'Show' instance converts the integer value of a signal to a letter for
-- reference. This follows the alphabet for the first 26 signals, and then will
-- be referred to as 'S' and the integer value after this.
instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

-- Keep the data Signal line in sync with the one above!
-- | Temporary module to help us use any number of signals in the user's
-- circuit. Otherwise, we would be bound to a number of arguments
-- (signals) known at compile time.
signalsApply :: Int -> [String]
signalsApply num = [
    "import Data.Char",
    "data Signal = Signal Int deriving Eq",
    "signs = [Signal i | i <- [0.." ++ show (num-1) ++ "]]",
    "apply c = c " ++ unwords ["(signs !! " ++ show i ++")" | i <- [0..num-1]]]

-- | Write to the temporary file.
writeTmpFile :: [String] -> IO ()
writeTmpFile ls =
    writeFile tmpModuleFile $ unlines withModule
      where withModule = "module Helper where" : ls

-- | Will remove the 'Helper' temporary module file.
-- If it does not exist then there is an error.
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Exported names in the user's haskell module (file).
circuitName, tmpModuleFile :: String
circuitName   = "component"
tmpModuleFile = ".Helper.hs"

-- | Helper functions because we deal with String, not Text.
count :: String -> String -> Int
count sub str = Text.count (Text.pack sub) (Text.pack str)

-- | Repeats a given string a given number of times.
strRepeat :: Int -> String -> String
strRepeat n str = Text.unpack $ Text.replicate n (Text.pack str)

-- | Loads the concept file and the included files into the
-- runtime Haskell interpreter.
loadModulesTopLevel :: [String] -> GHC.Interpreter ()
loadModulesTopLevel paths = do
    GHC.loadModules paths
    mods <- GHC.getLoadedModules
    GHC.setTopLevelModules mods

{- TODO: much of this is duplicated -}
-- | This performes much of the translation. It begins by loading the concept
-- file and the included files into the interpreter, then prepares the helper
-- file and imports this. It interprets the signals and then interprets the
-- concept itself (as a function). This can then be used for translation.
-- If the options given when calling the tool included the FSM flag, this
-- will be translated to an FSM, otherwise, translated to an STG.
doWork :: Bool -> [String] -> (String -> IO ()) -> GHC.Interpreter ()
doWork transFSM paths output = do
    {- Load user's module to gather info. -}
    loadModulesTopLevel paths
    {- Use the circuit's type to gather how many signals it takes. -}
    t <- GHC.typeOf circuitName
    let numSigns = count "->" t
    {- Load the generated module too. -}
    GHC.liftIO $ writeTmpFile $ signalsApply numSigns
    loadModulesTopLevel (paths ++ [tmpModuleFile])
    GHC.liftIO $ removeIfExists tmpModuleFile
    {- Fetch our signals. -}
    signs <- GHC.interpret "signs" (GHC.as :: [Signal])
    {- Obtain the circuit in terms of any signal (takes them as args). -}
    let ctype = strRepeat numSigns "Signal ->" ++ "CircuitConcept Signal"
    if transFSM
      then FSM.translateFSM circuitName ctype signs output
      else STG.translateSTG circuitName ctype signs output
    return ()
