module Main (main) where

import Data.Char
import qualified Data.Text as Text
import System.Directory
import Control.Exception
import System.Environment
import System.IO.Error

import Tuura.Concept
import Tuura.Concept.Simulation

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
data DynSignal = Signal Int deriving Eq

instance Show DynSignal where show (Signal i) = [chr (ord 'A' + i)]

{- Temporary module to help us use any number of signals in the user's
 - circuit. Otherwise, we would be bound to a number of arguments
 - (signals) known at compile time.
 - Keep the data DynSignal line in sync with the one above! -}
signalsApply :: Int -> [String]
signalsApply num = [
    "import Data.Char",
    "data DynSignal = Signal Int deriving Eq",
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

doWork :: String -> GHC.Interpreter ()
doWork path = do
    {- Load user's module to gather info. -}
    loadModulesTopLevel [path]
    {- Use the circuit's type to gather how many signals it takes. -}
    t <- GHC.typeOf circuitName
    let numSigns = count "->" t
    liftIO $ putStrLn $ "Circuit signal count: " ++ show numSigns
    {- Load the generated module too. -}
    liftIO $ writeTmpFile $ signalsApply numSigns
    loadModulesTopLevel [path, tmpModuleFile]
    liftIO $ removeIfExists tmpModuleFile
    {- Fetch our signals. -}
    signs <- GHC.interpret "signs" (GHC.as :: [DynSignal])
    liftIO $ putStrLn $ "Circuit signal names: " ++ show signs
    {- Obtain the circuit in terms of any signal (takes them as args). -}
    let ctype = strRepeat numSigns "DynSignal ->" ++ "CircuitConcept DynSignal"
    circuit <- GHC.unsafeInterpret circuitName ctype
    {- Use our generated code to apply our signals to the circuit above -}
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype ++ ") -> CircuitConcept DynSignal"
    let fullCircuit = apply circuit
    {- Input initial state. -}
    initialState <- liftIO $ readState numSigns
    liftIO $ putStrLn ""
    {- Run the interactive simulation. -}
    (_, finalState) <- liftIO $ runSimulation (doSimulate signs fullCircuit) initialState
    liftIO $ putStrLn $ "\nFinal state: " ++ showState finalState signs

isBinary :: String -> Bool
isBinary = foldr (\c -> (&&) (c == '0' || c == '1')) True

readState :: Int -> IO (State DynSignal)
readState num = do
    putStr "Initial state: "
    word <- getLine
    if length word /= num || not (isBinary word)
        then do
            let start = replicate num '0'
            let end = replicate num '1'
            let ranging = "from " ++ start ++ " to " ++ end
            putStrLn $ "Invalid state! Use a binary array ranging " ++ ranging
            readState num
        else return $ State (\(Signal i) -> '1' == word !! i)

{- Helper functions to avoid needing SignalWrapper to be Enum/Bounded -}
showState :: State t -> [t] -> String
showState (State v) = map (\s -> if v s then '1' else '0')

allTrans :: [a] -> [Transition a]
allTrans signs = [Transition s b | s <- signs, b <- [False, True]]

enabledTrans :: Monad m => s -> Concept s (Transition a) -> [a] -> m [Transition a]
enabledTrans st c signs =
    return $ filter (\t -> excited c t st) $ allTrans signs

{- Find the signal with a matching name. -}
readSignal :: [DynSignal] -> String -> DynSignal
readSignal (s:ls) word = if show s == word then s else readSignal ls word
readSignal [] _ = Signal (-1)

doSimulate :: MonadIO m => [DynSignal] -> Concept (State DynSignal) (Transition DynSignal) -> StateT (State DynSignal) m ()
doSimulate signs circuit = do
    st <- get
    liftIO $ putStrLn $ "State: " ++ showState st signs
    ts <- enabledTrans st circuit signs
    liftIO $ putStrLn $ "Enabled: " ++ show ts
    liftIO $ putStr "Do: "
    word <- liftIO getLine
    unless ("end" == map toLower word) $ do
        let signWord = init word
        let up = '+' == last word
        let sign = readSignal signs signWord
        let t = Transition sign up
        if t `elem` ts
            then fire t
            else liftIO $ putStrLn "Invalid transition!"
        doSimulate signs circuit
