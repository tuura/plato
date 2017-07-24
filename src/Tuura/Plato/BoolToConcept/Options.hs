module Tuura.Plato.BoolToConcept.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optSet     :: String
    , optReset   :: String
    , optOutput  :: String -> IO ()
    , optHelp    :: Bool }

defaultOptions :: Options
defaultOptions  = Options
    { optSet    = []
    , optReset  = []
    , optOutput = putStrLn
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
 , Option ['h'] ["help"]
     (NoArg (\ opts -> return opts { optHelp = True }))
     "Show this help message"
 ]

getOptions :: IO Options
getOptions = do
   argv <- getArgs
   result <- case getOpt Permute options argv of
      -- (o, [n], []  ) -> ioError (userError ("Unknown flag\n" ++ helpMessage ++ n))
      (o, [],  []  ) -> foldlM (flip id) defaultOptions o
      -- (_, _  , []  ) -> ioError (userError ("\nToo many input files\n" ++ helpMessage))
      (_, _  , errs) -> ioError (userError (concat errs ++ helpMessage))
   return $ result

helpMessage :: String
helpMessage = usageInfo header options
  where
    header = "Usage: " ++ runCommand ++ " [input file] [OPTION...]"
    runCommand = "stack runghc boolToConcept/ --"
