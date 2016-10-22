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

import Tuura.Concept.STG
import Tuura.Concept.STG.Simulation
import Tuura.Concept.STG.Translation hiding (Signal)

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Exactly one path needed"
        else do
            r <- GHC.runInterpreter $ doWork (head args)
            case r of
               Left err -> putStrLn $ displayException err
               Right () -> return ()

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
    "apply c = c " ++ unwords ["(signs !! " ++ show i ++ ")" | i <- [0..num-1]]]

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

doWork :: String -> GHC.Interpreter () {- TODO: much of this is duplicated -}
doWork path = do
    {- Load user's module to gather info. -}
    loadModulesTopLevel [path]
    {- Use the circuit's type to gather how many signals it takes. -}
    t <- GHC.typeOf circuitName
    let numSigns = count "->" t
    {- Load the generated module too. -}
    liftIO $ writeTmpFile $ signalsApply numSigns
    loadModulesTopLevel [path, tmpModuleFile]
    liftIO $ removeIfExists tmpModuleFile
    {- Fetch our signals. -}
    signs <- GHC.interpret "signs" (GHC.as :: [Signal])
    {- Obtain the circuit in terms of any signal (takes them as args). -}
    let ctype = strRepeat numSigns "Signal ->" ++ "CircuitConcept Signal"
    circuit <- GHC.unsafeInterpret circuitName ctype
    {- Use our generated code to apply our signals to the circuit above -}
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype ++ ") -> CircuitConcept Signal"
    let fullCircuit = apply circuit
    let translation = liftIO $ putStr $ translate signs fullCircuit
    (_, _) <- liftIO $ runSimulation translation (State $ const False)
    return ()
