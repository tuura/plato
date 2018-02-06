-----------------------------------------------------------------------------
-- |
-- Module     : Tuura.Concept.STG.Translation
-- Copyright  : (c) 2015-2018, Tuura authors
-- License    : BSD (see the file LICENSE)
-- Maintainer : jonathan.r.beaumont@gmail.com
-- Stability  : experimental
--
-- Plato is a tool which embeds the Asynchronous Concepts language in Haskell.
-- This language is used for the specification of asynchronous circuits, and
-- is fully compositional and highly reusable, from individual concepts to
-- entire concepts specifications.

-- Plato can also compile and validate Asynchronous Concepts, with the help of
-- the GHC. Compiled concepts can then be translated to existing modelling
-- formalisms of Signal Transition Graphs (STGs) and State Graphs. These models
-- feature a long history of theory and therefore several tools which can be
-- used for verification and synthesis. STGs and State Graphs can be visualized
-- in Workcraft (https://workcraft.org), where Plato and the tools for these
-- models are all integrated.
--
-- This module defines several functions which are used specificall for the
-- translation of Asynchronous Concepts to STGs.
--
-----------------------------------------------------------------------------

module Tuura.Concept.STG.Translation where

import Data.List.Extra
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty, groupAllWith)
import Text.Printf

import Tuura.Plato.Translate.Translation
import Tuura.Boolean

import Tuura.Concept.STG

-- | These imported modules are from the 'Hint' package, a Runtime Haskell
-- Interpreter. This is used to compile a concept specification once
-- validation has been successful.
import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

-- | Prepares the Haskell interpreter to compile the concept specification.
translateSTG :: (Show a, Ord a) => String -> String -> [a] -> (String -> IO ()) -> GHC.Interpreter()
translateSTG circuitName ctype signs out = do
    circ <- GHC.unsafeInterpret circuitName ctype
    apply <- GHC.unsafeInterpret "apply" $
        "(" ++ ctype ++ ") -> CircuitConcept Signal"
    let circuit = apply circ
    let result = translate circuit signs
    if fst result
      then GHC.liftIO $ out (snd result)
      else GHC.liftIO $ putStrLn (snd result)

-- | Takes a concept specification, validates it, and if valid, will translate
-- it to an STG providing the STG in .g format.
-- If validation fails, it will return these errors.
translate :: (Show a, Ord a) => CircuitConcept a -> [a] -> (Bool, String)
translate circuit signs =
    case validate signs circuit of
        Valid -> do
            let initStrs = map (\s -> (show s, getDefined $ getInit s)) signs
                allArcs = arcLists (arcs circuit)
                groupByEffect = groupAllWith snd allArcs
                arcStrs = nubOrd (concatMap handleArcs groupByEffect)
                invStr = map genInvStrs (invariant circuit)
                inputSigns = filter ((==Input) . interface circuit) signs
                outputSigns = filter ((==Output) . interface circuit) signs
                internalSigns = filter ((==Internal) . interface circuit) signs
            (True, genSTG inputSigns outputSigns internalSigns arcStrs initStrs invStr)
        Invalid errs -> (False, addErrors errs)
  where
    getInit = initial circuit

-- | Apply cartesian product to causalities to provide us with a list of arcs.
-- Concept specification provide causalities in CNF, but for translation,
-- needs the causalities in DNF. The cartesian product converts these.
-- Due to the caller, xs will never be empty, so 'snd (head xs)' never fails.
handleArcs :: (Ord a, Show a) => NonEmpty ([Transition a], Transition a) -> [String]
handleArcs xs = addConsistencyTrans effect n ++ concatMap transition arcMap
  where
    effect = snd (NonEmpty.head xs)
    effectCauses = NonEmpty.toList $ NonEmpty.map fst xs
    dnfCauses = simplifyDNF . convertCNFtoDNF . simplifyCNF $ CNF (map toLiteral effectCauses)
    transCauses = map toTransitions (fromDNF dnfCauses)
    n = length transCauses
    arcMap = concatMap (`arcPairs` effect) zipCauseNos
    zipCauseNos = zip transCauses [0..(n - 1)]

-- | Takes the input, output and internal signals, the generated strings for
-- the arcs, the initial states and the invariant strings, and generates the .g
-- output for the translated STG.
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

-- | For each additional transition object for a signal, caused by OR
-- causality, add this to the signals transition loop.
addConsistencyTrans :: Show a => Transition a -> Int -> [String]
addConsistencyTrans effect n
    | newValue effect = map (\x ->
      printf "%s0 %s/%s\n" (init (show effect)) (show effect) (show x) ++
      printf "%s/%s %s1" (show effect) (show x) (init (show effect)))
      [1..n - 1]
    | otherwise = map (\x ->
      printf "%s1 %s/%s\n" (init (show effect)) (show effect) (show x) ++
      printf "%s/%s %s0" (show effect) (show x) (init (show effect)))
      [1..n - 1]

-- | Applies a number to transitions in the event of multiple possible causes,
-- due to OR causality.
arcPairs :: Show a => ([a], Int) -> a -> [(a, String)]
arcPairs (causes, n) effect
    | n == 0 = map (\c -> (c, show effect)) causes
    | otherwise = map (\d -> (d, show effect  ++ "/" ++ show n)) causes

-- | Connects the place after an effect transition to the cause transition.
transition :: Show a => (Transition a, String) -> [String]
transition (f, t)
    | newValue f = readArc (init (show f) ++ "1") t
    | otherwise  = readArc (init (show f) ++ "0") t

-- | .g template. Each "%s" will be replaced with a string.
tmpl :: String
tmpl = unlines [".model out",
                ".inputs %s",
                ".outputs %s",
                ".internal %s",
                ".graph",
                "%s.marking {%s}",
                ".end",
                "%s"]

-- | Provides a list of all signals.
output :: [(String, Bool)] -> [String]
output = nubOrd . map fst

-- | Create a signal transition loop for a signal, so each signal can transition
-- both high and low in the system, satisfying the propert of consisteny.
consistencyLoop :: String -> [String]
consistencyLoop s = map (\f -> printf f s s)
                    ["%s0 %s+", "%s+ %s1", "%s1 %s-", "%s- %s0"]

-- | Provides strings for the initial state.
initVals :: [String] -> [(String, Bool)] -> [String]
initVals l symbs = concatMap (\s -> [printf "%s%i" s $ initVal s symbs]) l

-- | Finds the 1 or 0 value for the initial state.
initVal :: String -> [(String, Bool)] -> Int
initVal s ls = sum (map getValue ls)
  where
    getValue x = if fst x == s
                 then fromEnum (snd x)
                 else 0

-- | Creates a double ended arc for a connection, connecting the two objects
-- in both directions. This double ended arc is a read-arc, which allows the
-- effect transition to occur, without consuming the token in the cause place.
readArc :: String -> String -> [String]
readArc f t = [f ++ " " ++ t, t ++ " " ++ f]

-- | Creates a report for the invariant, to be placed at the end of the STG
-- output. This can be used for automated verification by Workcraft.
genInvStrs :: Show a => Invariant (Transition a) -> String
genInvStrs (NeverAll es)
        | null es   = []
        | otherwise = "invariant = not (" ++
                      intercalate " && " (map format es) ++
                      ")"
  where
    format e = if newValue e
               then show (signal e)
               else "not " ++ show (signal e)
