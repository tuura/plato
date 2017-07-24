module Tuura.Plato.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optInput   :: String
    , optOutput  :: String -> IO ()
    , optInclude :: [String]
    , optFSM     :: Bool
    , optBool    :: Bool
    , optHelp    :: Bool }

defaultOptions :: Options
defaultOptions   = Options
    { optInput   = ""
    , optOutput  = putStrLn
    , optInclude = []
    , optFSM     = False
    , optBool    = False
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
 , Option ['b'] ["boolean"]
     (NoArg (\ opts -> return opts { optBool = True }))
     "Create concept specification from Boolean set and reset functions"
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
   _ <- validateOptions result
   return $ result

    -- result <- case getOpt Permute options argv of
    --     (opts, [] , []  ) -> foldlM (flip id) defaultOptions opts
    --     (opts, [f], []  ) -> foldlM (flip id)
    --                          defaultOptions { optInput = readFile f } opts
    --     (_   , _  , []  ) -> ioError $ userError "Multiple input files"
    --     (_   , _  , errs) -> ioError . userError $ concat errs

helpMessage :: String
helpMessage = usageInfo header options
  where
    header = "Usage: " ++ runCommand ++ " [input file] [OPTION...]"
    runCommand = "stack runghc translate/Main.hs --"

validateOptions :: Options -> IO Options
validateOptions ops
    | (optInput ops /= "") && (optBool ops)   = ioError (userError
      ("\nCannot translate a specification and generate a specification\n" ++
      "from set and reset functions at the same time.\n" ++
      "Remove the input filepath\n." ++ helpMessage))
    | (optInclude ops /= []) && (optBool ops) = ioError (userError
      ("\nIncludes cannot be used when generating a specification from\n" ++
      "from set and reset functions at the same time\n" ++
      "Remove the include flag and filepath.\n" ++ helpMessage))
    | (optFSM ops) && (optBool ops)           = ioError (userError
      ("\nGenerating a concept specification from set and reset functions\n" ++
       "does not translate to FSM or STG specifically.\n" ++
       "Remove the FSM translation flag.\n" ++ helpMessage))
    | otherwise = return $ ops
