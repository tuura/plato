module Main (main) where

import Prelude hiding (not)

import Tuura.Plato.BoolToConcept
import Tuura.Plato.BoolToConcept.Options

main :: IO ()
main = do
    options <- getOptions
    let effect = optEffect options
    if null $ optSet options then do
        putStr "set function:   "
        set <- getLine
        putStr "reset function: "
        reset <- getLine
        putStrLn ""
        output (doWork set reset effect) (optOutput options)
    else do
        let set = optSet options
            reset = optReset options
        output (doWork set reset effect) (optOutput options)

doWork :: String -> String -> String -> (Bool, String)
doWork set reset effect =
    if null reset
      then fromFunctions set ("!(" ++ set ++ ")") effect
      else fromFunctions set reset effect

output :: (Bool, String) -> (String -> IO ()) -> IO ()
output (pass, result) out =
    if pass
      then out result
      else putStrLn result
