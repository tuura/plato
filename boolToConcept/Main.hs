module Main (main) where

import Prelude
import Tuura.Parser.Boolean
import Data.List
import Data.List.Extra
import System.IO

main :: IO ()
main = do
    putStr "set function:   "
    hFlush stdout
    setString   <- getLine
    putStr "reset function: "
    hFlush stdout
    resetString <- getLine
    let setResult = parseExpr setString
    let resetResult = parseExpr resetString
    case setResult of
         Left err -> do
           putStr "parse error at "
           print err
         Right x -> putStr ""
    case resetResult of
         Left err -> do
            putStr "parse error at "
            print err
         Right x -> putStr ""
    let set = right setResult
    let reset = right resetResult
    let allVars = nub $ (listVars set) ++ (listVars reset)
    putStrLn $ createConceptSpec allVars set reset
  where
    right (Right x) = x

createConceptSpec :: [String] -> Expr -> Expr -> String
createConceptSpec vars set reset = do
    let mod        = "module Concept where\n\n"
        circuit    = "circuit " ++ unwords vars ++ " out = "
        topConcept = "outRise <> outFall <> interface <> initialState\n"
        wh         = "  where\n"
        outRise    = "    outRise = \n" ++ generateConcepts set
    mod ++ circuit ++ topConcept ++ wh ++ outRise

generateConcepts :: Expr -> String
generateConcepts (And a b) = "(" ++ generateConcepts a ++ ")" ++
                  " AND " ++ "(" ++ generateConcepts b ++ ")"
generateConcepts (Or a b) = generateConcepts a ++ " OR " ++ generateConcepts b
generateConcepts (SubExpr a) = "[" ++ generateConcepts a ++ "] "
generateConcepts (Var a) = a
generateConcepts (Not (Var a)) = "!" ++ a
