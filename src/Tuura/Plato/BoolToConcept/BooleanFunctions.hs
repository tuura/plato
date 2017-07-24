module Tuura.Plato.BoolToConcept.BooleanFunctions (
  fromFunctions) where

import Tuura.Parser.Boolean
import Data.List

fromFunctions :: String -> String -> (Bool, String)
fromFunctions setString resetString = do
    let setResult = parseExpr setString
    let resetResult = parseExpr resetString
    if (left setResult /= "")
      then (False, "parse error at " ++ left setResult)
    else if (left resetResult /= "")
      then (False, "parse error at " ++ left resetResult)
      else do
      let set = right setResult
      let reset = right resetResult
      let allVars = nub $ (listVars set) ++ (listVars reset)
      (True, createConceptSpec allVars set reset)
  where
    right (Right x) = x
    right (Left _) = right (parseExpr "")
    left  (Left x) = show x
    left _ = ""

createConceptSpec :: [String] -> Expr -> Expr -> String
createConceptSpec vars set reset = modName ++ imp
                                ++ circuit ++ topConcept ++ wh
                                ++ outRise ++ outFall
                                ++ inInter ++ outInter
                                ++ initState
    where
      modName    = "\nmodule Concept where \n\n"
      imp        = "import Tuura.Concept.STG \n\n"
      circuit    = "circuit " ++ unwords vars ++ " out = "
      topConcept = "outRise <> outFall <> interface <> initialState\n"
      wh         = "  where\n"
      rConcept   = intersperse "<>" $ map (genConcepts True) (listOrs set)
      outRise    = "    outRise = " ++ unwords rConcept
      fConcept   = intersperse "<>" $ map (genConcepts False) (listOrs reset)
      outFall    = "\n    outFall = " ++ unwords fConcept
      inputVars  = intersperse "," vars
      inInter    = "\n    interface = inputs [" ++ unwords inputVars ++ "]"
      outInter   = " <> outputs [out]"
      initState  = "\n    initialState = "
                ++ "initialise0 [" ++ unwords inputVars ++ " , out]"

genConcepts :: Bool -> [Expr] -> String
genConcepts v e
    | (length e) == 1 = causes ++ op ++ effect
    | otherwise       = "[" ++ causes ++ "]" ++ op ++ effect
  where
    causes = unwords $ intersperse "," $ map (direction) e
    direction (Var a)       = "rise " ++ a
    direction (Not (Var a)) = "fall " ++ a
    direction a = "error " ++ show a
    op       = if (length e > 1) then " ~|~> " else " ~> "
    effect   = if (v) then "rise out" else "fall out"

listOrs :: Expr -> [[Expr]]
listOrs (And a b) = listOrs a ++ listOrs b
listOrs (Or a b) = [orVars (Or a b)]
listOrs (SubExpr a) = listOrs a
listOrs a = [[a]]

orVars :: Expr -> [Expr]
orVars (Or a b) = orVars a ++ orVars b
orVars (SubExpr a) = orVars a
orVars a = [a]
