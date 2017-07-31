module Tuura.Plato.BoolToConcept.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optSet     :: String
    , optReset   :: String
    , optOutput  :: String -> IO ()
    , optEffect  :: String
    , optHelp    :: Bool }

defaultOptions :: Options
defaultOptions  = Options
    { optSet    = []
    , optReset  = []
    , optOutput = putStrLn
    , optEffect = "out"
    , optHelp   = False }

options :: [OptDescr (Options -> IO Options)]
options =
 [ Option ['s'] ["set"]
     (ReqArg (\ s opts -> return opts { optSet = s })
      "FUNCTION")
      "Set function"
 , Option ['r'] ["reset"]
     (ReqArg (\ r opts -> return opts { optReset = r })
      "FUNCTION")
      "Reset function"
 , Option ['o'] ["output"]
     (ReqArg (\ f opts -> return opts { optOutput = writeFile f }) "FILE")
     "Write output to a file"
 , Option ['e'] ["effect-signal"]
     (ReqArg (\ e opts -> return opts { optEffect = e }) "STRING")
     "Name of output signal from set/reset functions"
 , Option ['h'] ["help"]
     (NoArg (\ opts -> return opts { optHelp = True }))
     "Show this help message"
 ]

getOptions :: IO Options
getOptions = do
   argv <- getArgs
   result <- case getOpt Permute options argv of
      (o, [],  []  ) -> foldlM (flip id) defaultOptions o
      (_, _  , errs) -> ioError (userError (concat errs ++ helpMessage))
   _ <- validateOptions result
   return result

validateOptions :: Options -> IO Options
validateOptions ops
  | null (optSet ops) && optReset ops /= [] = ioError
      (userError ("A reset function cannot be used on it's own.\n" ++
                  "Please provide just a set function, or both a set and " ++
                  "reset function.\n" ++ helpMessage))
  | otherwise = return ops

helpMessage :: String
helpMessage = usageInfo header options
  where
    header = "Usage: " ++ runCommand ++ " [input file] [OPTION...]"
    runCommand = "stack runghc boolToConcept/ --"
