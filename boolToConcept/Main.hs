module Main (main) where

import Prelude hiding (not)

import Tuura.Plato.BoolToConcept
import Tuura.Plato.BoolToConcept.Options

main :: IO ()
main = do
    options <- getOptions
    if (optSet options == []) then do
        putStr "set function:   "
        set <- getLine
        putStr "reset function: "
        reset <- getLine
        output (doWork set reset) (optOutput options)
    else do
        let set = optSet options
        let reset = optReset options
        output (doWork set reset) (optOutput options)

doWork :: String -> String -> (Bool, String)
doWork set reset =
    if (reset == [])
      then fromFunctions set ("!(" ++ set ++ ")")
      else fromFunctions set reset

output :: (Bool, String) -> (String -> IO ()) -> IO ()
output (pass, result) out = if (pass)
    then out $ result
    else putStrLn result
