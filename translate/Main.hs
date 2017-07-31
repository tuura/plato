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

import qualified Tuura.Concept.FSM as FSM hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.FSM.Simulation as FSM
import qualified Tuura.Concept.FSM.Translation as FSM

import qualified Tuura.Concept.STG as STG hiding (Concept, CircuitConcept)
import qualified Tuura.Concept.STG.Simulation as STG
import qualified Tuura.Concept.STG.Translation as STG

import Tuura.Plato.Translate.Options

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

main :: IO ()
main = do
    options <- getOptions
    let input = optInput options
    let paths = [input] ++ optInclude options
    r <- GHC.runInterpreter $ doWork (optFSM options) paths (optOutput options)
    either (putStrLn . displayException) return r

{- Our own Signal type. Contains the signal index, from 0 to x-1 if
 - there are x signals. -}
data Signal = Signal Int deriving Eq

--instance Show Signal where show (Signal i) = [chr (ord 'A' + i)]
instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

{- Temporary module to help us use any number of signals in the user's
 - circuit. Otherwise, we would be bound to a number of arguments
 - (signals) known at compile time.
 - Keep the data Signal line in sync with the one above! -}
signalsApply :: Int -> [String]
signalsApply num = [
    "import Data.Char",
    "data Signal = Signal Int deriving Eq",
    "signs = [Signal i | i <- [0.." ++ show (num-1) ++ "]]",
    "apply c = c " ++ unwords ["(signs !! " ++ show i ++")" | i <- [0..num-1]]]

writeTmpFile :: [String] -> IO ()
writeTmpFile ls =
    writeFile tmpModuleFile $ unlines withModule
      where withModule = "module Helper where" : ls

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

{- Exported names in the user's haskell module (file) -}
circuitName, tmpModuleFile :: String
circuitName   = "circuit"
tmpModuleFile = ".Helper.hs"

{- Helper functions because we deal with String, not Text. -}
count :: String -> String -> Int
count sub str = Text.count (Text.pack sub) (Text.pack str)

strRepeat :: Int -> String -> String
strRepeat n str = Text.unpack $ Text.replicate n (Text.pack str)

loadModulesTopLevel :: [String] -> GHC.Interpreter ()
loadModulesTopLevel paths = do
    GHC.loadModules paths
    mods <- GHC.getLoadedModules
    GHC.setTopLevelModules mods

{- TODO: much of this is duplicated -}
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
