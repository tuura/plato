module Tuura.Plato.Options (Options(..), getOptions) where

import System.Console.GetOpt
import System.Environment

data Options = Options
    { optInput   :: String
    , optInclude :: [String]
    , optFSM     :: Bool
    , optHelp    :: Bool }

defaultOptions :: Options
defaultOptions   = Options
    { optInput   = ""
    , optInclude = []
    , optFSM     = False
    , optHelp    = False }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['i'] ["include"]
     (ReqArg (\ d opts -> opts { optInclude = optInclude opts ++ [d] }) "FILEPATH")
     "Concept file to be included"
 , Option ['f'] ["fsm"]
     (NoArg (\ opts -> opts { optFSM = True }))
     "Translate concept specification to an FSM"
 , Option ['h'] ["help"]
     (NoArg (\ opts -> opts { optHelp = True }))
     "Show this help message"
 ]

getOptions :: IO Options
getOptions = do
   argv <- getArgs
   result <- case getOpt Permute options argv of
      (_, [] , _   ) -> ioError (userError ("\nNo input file given\n" ++ helpMessage))
      (o, [n], []  ) -> return (foldl (flip id) defaultOptions {optInput = n} o)
      (_, _  , []  ) -> ioError (userError ("\nToo many input files\n" ++ helpMessage))
      (_, _  , errs) -> ioError (userError (concat errs ++ helpMessage))
   return result
    where
      helpMessage = usageInfo header options
      header = "Usage: " ++ "plato" ++ " [input file] [OPTION...]"
