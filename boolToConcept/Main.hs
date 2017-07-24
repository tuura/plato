module Main (main) where

import Tuura.Plato.BooleanFunctions

main :: IO ()
main = do
    -- options <- getOptions
    putStr "set function:   "
    set <- getLine
    putStr "reset function: "
    reset <- getLine
    let result = fromBooleanFunctions set reset
    if (fst result)
      then putStrLn $ snd result
      else putStrLn $ snd result