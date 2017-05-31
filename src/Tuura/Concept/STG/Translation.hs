module Tuura.Concept.STG.Translation where

import Data.List.Extra
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty, groupAllWith)
import Text.Printf

import Tuura.Plato.Translation

import Tuura.Concept.STG

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

translateSTG :: (Show a, Ord a) => String -> String -> [a] -> GHC.Interpreter()
translateSTG circuitName ctype signs = do
    circ <- GHC.unsafeInterpret circuitName ctype
    apply <- GHC.unsafeInterpret "apply" $
        "(" ++ ctype ++ ") -> CircuitConcept Signal"
    let circuit = apply circ
    GHC.liftIO $ putStr (translate circuit signs)

translate :: (Show a, Ord a) => CircuitConcept a -> [a] -> String
translate circuit signs =
    case validate signs circuit of
        Valid -> do
            let initStrs = map (\s -> (show s, (getDefined $ getInit s))) signs
                allArcs = arcLists (arcs circuit)
                groupByEffect = groupAllWith snd allArcs
                arcStrs = nubOrd (concatMap handleArcs groupByEffect)
                invStr = map genInvStrs (invariant circuit)
                inputSigns = filter ((==Input) . interface circuit) signs
                outputSigns = filter ((==Output) . interface circuit) signs
                internalSigns = filter ((==Internal) . interface circuit) signs
            genSTG inputSigns outputSigns internalSigns arcStrs initStrs invStr
        Invalid errs -> addErrors errs
  where
    getInit = initial circuit

-- Due to the caller, xs will never be empty, so `snd (head xs)` never fails.
handleArcs :: (Ord a, Show a) => NonEmpty ([Transition a], Transition a) -> [String]
handleArcs xs = addConsistencyTrans effect n ++ concatMap transition arcMap
  where
    effect = snd (NonEmpty.head xs)
    effectCauses = NonEmpty.map fst xs
    transCauses = cartesianProduct effectCauses
    n = length transCauses
    arcMap = concatMap (\m -> arcPairs m effect) zipCauseNos
    zipCauseNos = zip transCauses [0..(n - 1)]

genSTG :: Show a => [a] -> [a] -> [a] -> [String] -> [(String, Bool)]
            -> [String] -> String
genSTG inputSigns outputSigns internalSigns arcStrs initStrs invStr =
    printf tmpl (unwords ins)
                (unwords outs)
                (unwords ints)
                (unlines allArcs)
                (unwords marks)
                (unlines invStr)
  where
    allSigns = output initStrs
    outs = map show outputSigns
    ins = map show inputSigns
    ints = map show internalSigns
    allArcs = concatMap consistencyLoop allSigns ++ arcStrs
    marks = initVals allSigns initStrs

addConsistencyTrans :: Show a => Transition a -> Int -> [String]
addConsistencyTrans effect n
    | newValue effect = map (\x ->
      (printf "%s0 %s/%s\n" (init (show effect)) (show effect) (show x)) ++
      (printf "%s/%s %s1" (show effect) (show x) (init (show effect))))
      [1..n - 1]
    | otherwise = map (\x ->
      (printf "%s1 %s/%s\n" (init (show effect)) (show effect) (show x)) ++
      (printf "%s/%s %s0" (show effect) (show x) (init (show effect))))
      [1..n - 1]

arcPairs :: Show a => ([a], Int) -> a -> [(a, String)]
arcPairs (causes, n) effect
    | n == 0 = map (\c -> (c, show effect)) causes
    | otherwise = map (\d -> (d, (show effect  ++ "/" ++ show n))) causes

transition :: Show a => (Transition a, String) -> [String]
transition (f, t)
    | newValue f = readArc (init (show f) ++ "1") t
    | otherwise  = readArc (init (show f) ++ "0") t

tmpl :: String
tmpl = unlines [".model out",
                ".inputs %s",
                ".outputs %s",
                ".internal %s",
                ".graph",
                "%s.marking {%s}",
                ".end",
                "%s"]

output :: [(String, Bool)] -> [String]
output = nubOrd . map fst

consistencyLoop :: String -> [String]
consistencyLoop s = map (\f -> printf f s s)
                    ["%s0 %s+", "%s+ %s1", "%s1 %s-", "%s- %s0"]

initVals :: [String] -> [(String, Bool)] -> [String]
initVals l symbs = concat (map (\s -> [printf "%s%i" s $ initVal s symbs]) l)

initVal :: String -> [(String, Bool)] -> Int
initVal s ls = sum (map getValue ls)
  where
    getValue x = if (fst x == s)
                 then fromEnum (snd x)
                 else 0

readArc :: String -> String -> [String]
readArc f t = [f ++ " " ++ t, t ++ " " ++ f]

genInvStrs :: (Ord a, Show a) => Invariant (Transition a) -> String
genInvStrs (NeverAll es)
        | es        == [] = []
        | otherwise = "invariant = not (" ++
                      (intercalate " && " (map format es)) ++
                      ")"
  where
    format e = if (newValue e)
               then show (signal e)
               else "not " ++ show (signal e)
