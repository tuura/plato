module Tuura.Plato.Translate.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optInput   :: String
    , optOutput  :: String -> IO ()
    , optInclude :: [String]
    , optFSM     :: Bool
    , optHelp    :: Bool }

defaultOptions :: Options
defaultOptions   = Options
    { optInput   = ""
    , optOutput  = putStrLn
    , optInclude = []
    , optFSM     = False
    , optHelp    = False }

options :: [OptDescr (Options -> IO Options)]
options =
 [ Option ['i'] ["include"]
     (ReqArg (\ d opts -> return opts { optInclude = optInclude opts ++ [d] })
      "FILEPATH")
      "Concept file to be included"
 , Option ['o'] ["output"]
     (ReqArg (\ f opts -> return opts { optOutput = writeFile f }) "FILE")
     "Write output to a file"
 , Option ['f'] ["fsm"]
     (NoArg (\ opts -> return opts { optFSM = True }))
     "Translate concept specification to an FSM"
 , Option ['h'] ["help"]
     (NoArg (\ opts -> return opts { optHelp = True }))
     "Show this help message"
 ]

getOptions :: IO Options
getOptions = do
   argv <- getArgs
   result <- case getOpt Permute options argv of
      (o, [n], []  ) -> foldlM (flip id) defaultOptions {optInput = n} o
      (o, [],  []  ) -> foldlM (flip id) defaultOptions o
      (_, [] , _   ) -> ioError (userError ("\nNo input file given\n" ++ helpMessage))
      (_, _  , []  ) -> ioError (userError ("\nToo many input files\n" ++ helpMessage))
      (_, _  , errs) -> ioError (userError (concat errs ++ helpMessage))
   return result

helpMessage :: String
helpMessage = usageInfo header options
  where
    header = "Usage: " ++ runCommand ++ " [input file] [OPTION...]"
    runCommand = "stack runghc translate/Main.hs --"
