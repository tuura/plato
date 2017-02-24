module Tuura.Concept.FSM.Translation where

import Data.List
import Data.List.Extra
import Data.Ord (comparing)
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

import Tuura.Concept.FSM

import Tuura.Plato.Translation

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

-- Type for "dont care" transitions
type CausalityX a = [([TransitionX a], Transition a)]

-- Transition type using Tristate. Used for initial construction of arcs.
data TransitionX a = TransitionX
    {
        msignal   :: a,
        mnewValue :: Tristate -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (TransitionX a) where
    show (TransitionX s (Tristate (Just True) )) = show s ++ "+"
    show (TransitionX s (Tristate (Just False))) = show s ++ "-"
    show (TransitionX s (Tristate Nothing     )) = show s ++ "x"

data Tristate = Tristate (Maybe Bool)
    deriving Eq

instance Show Tristate where
    show (Tristate (Just True) ) = "1"
    show (Tristate (Just False)) = "0"
    show (Tristate Nothing     ) = "x"

triTrue :: Tristate
triTrue = Tristate (Just True)

triFalse :: Tristate
triFalse = Tristate (Just False)

triX :: Tristate
triX = Tristate Nothing

-- FSM arc type used during state expansion
data FsmArcX a = FsmArcX
    {
        srcEncx :: [Tristate],
        transx :: Transition a,
        destEncx :: [Tristate]
    }

instance Show a => Show (FsmArcX a) where
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " " ++ show tran ++ " " ++ show tenc ++ ")\n"

-- Final FSM arc type using Ints for state encoding
data FsmArc a = FsmArc
    {
        srcEnc :: Int,
        trans :: Transition a,
        destEnc :: Int
    }

instance Show a => Show (FsmArc a) where
    show (FsmArc senc tran tenc) = "s" ++ show senc ++ " " ++ show tran ++ " s" ++ show tenc

translateFSM :: (Show a, Ord a) => String -> String -> [a] -> GHC.Interpreter ()
translateFSM circuitName ctype signs = do
    circ <- GHC.unsafeInterpret circuitName ctype
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype ++ ") -> CircuitConcept Signal"
    let circuit = apply circ
    GHC.liftIO $ putStr (translate circuit signs)

translate :: (Show a, Ord a) => CircuitConcept a -> [a] -> String
translate circuit signs =
    case validate signs circuit of
      Valid -> do
          let allArcs = addConsistency (arcs circuit) signs
          let sortedArcs = concatMap handleArcs (groupSortOn snd allArcs)
          let invariants = concatMap (\i -> createInvariants i signs) (invariant circuit)
          let arcStrs = map show (createAllArcs sortedArcs invariants)
          let inputSigns = filter ((==Input) . interface circuit) signs
          let outputSigns = filter ((==Output) . interface circuit) signs
          let internalSigns = filter ((==Internal) . interface circuit) signs
          let initialState = getInitialState circuit signs
          genFSM inputSigns outputSigns internalSigns arcStrs initialState
      Invalid errs ->
          "Error. \n" ++ concatMap addErrors errs

getInitialState :: CircuitConcept a -> [a] -> String
getInitialState circuit signs = show (encToInt state)
  where
    state = map (\s -> if (getDefined $ initial circuit s) then triTrue else triFalse) signs

addConsistency :: Ord a => [([Transition a], Transition a)] -> [a] -> [([Transition a], Transition a)]
addConsistency allArcs signs = nubOrd (allArcs ++ concatMap (\s -> [([rise s], fall s), ([fall s], rise s)]) signs)

handleArcs :: [([Transition a], Transition a)] -> [([Transition a], Transition a)]
handleArcs arcLists = result
        where
            effect = snd (head arcLists)
            effectCauses = map fst arcLists
            transCauses = cartesianProduct effectCauses
            result = map (\m -> (m, effect)) transCauses

genFSM :: Show a => [a] -> [a] -> [a] -> [String] -> String -> String
genFSM inputSigns outputSigns internalSigns arcStrs initialState =
     printf tmpl (unwords ins) (unwords outs) (unwords ints) (unlines arcStrs) initialState
    where
        outs = map show outputSigns
        ins = map show inputSigns
        ints = map show internalSigns

tmpl :: String
tmpl = unlines [".inputs %s", ".outputs %s", ".internals %s", ".state graph", "%s.marking {s%s}", ".end"]

fullList :: ([a], a) -> [a]
fullList (l,t) = t:l

fullListm :: ([TransitionX a], Transition a) -> [TransitionX a]
fullListm (l,t) = (toTransitionX t):l

-- Given [([a], b)], remove all b from a
removeDupes :: Eq a => [([Transition a], Transition a)] -> [([Transition a], Transition a)]
-- (filter ((/= ((signal . snd) x)) . signal) (fst x), snd x)
removeDupes = map (ap ((,) . ap (filter . (. signal) . (/=) . signal . snd) fst) snd)

toTransitionX :: Transition a -> TransitionX a
toTransitionX = liftM2 TransitionX signal (Tristate . Just . newValue)

-- Extract signal list from transition list
onlySignals :: [[Transition a]] -> [[a]]
onlySignals = map (map signal)

-- Find all signal names in design
getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = sort . foldl union [] . onlySignals

addMissingSignals :: Ord a => [([Transition a], Transition a)] -> CausalityX a
addMissingSignals x = zip (zipWith (++) newTransitions oldTransitions) (map snd noDupes)
    where noDupes = removeDupes x
          oldTransitions = map (map toTransitionX . fst) noDupes
          newTransitions = ((map . map) (flip TransitionX triX) . missingSignals . transitionList) noDupes
          transitionList =  map fullList
          missingSignals y = map (getAllSignals y \\) (onlySignals y)

encode :: Ord a => [TransitionX a] -> [Tristate]
encode  = map mnewValue . sortTransitions
    where sortTransitions = sortBy (comparing msignal)

createArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createArcs xs = zipWith3 createArc makeSrcEncs makeDestEncs activeTransitions
    where createArc senc tenc xTrans = FsmArcX senc xTrans tenc
          makeDestEncs = a xs
          makeSrcEncs = (a . map flipTransition) xs
          a = map (encode . fullListm) . addMissingSignals
          flipTransition x = (fst x, (invert . snd) x)
          invert = liftM2 Transition signal (not . newValue)
          activeTransitions = (map snd .  addMissingSignals) xs

createInvariants :: Ord a => Invariant (Transition a) -> [a] -> [[Tristate]]
createInvariants (NeverAll es) allSigns = expand (encode newTransitions)
    where newTransitions = transX ++ (concatMap (map (\s -> TransitionX { msignal = s, mnewValue = triX })) missingSignals)
          transX = map toTransitionX es
          missingSignals = map (allSigns \\) (onlySignals [es])
          expand t = case elemIndex triX t of
               Nothing -> [t]
               Just n  -> do
                let newTrue = replaceAtIndex triTrue t n
                let newFalse = replaceAtIndex triFalse t n
                expand newTrue ++ expand newFalse

replaceAtIndex :: a -> [a] -> Int -> [a]
replaceAtIndex item ls n = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls

expandX :: FsmArcX a -> [FsmArcX a]
expandX xs = case elemIndex triX (srcEncx xs) of
               Nothing -> [xs]
               Just n  -> do
                let newTrue = makeArc (replaceAtIndex triTrue (srcEncx xs) n)
                                       (replaceAtIndex triTrue (destEncx xs) n)
                let newFalse = makeArc (replaceAtIndex triFalse (srcEncx xs) n)
                                  (replaceAtIndex triFalse (destEncx xs) n)
                expandX newTrue ++ expandX newFalse
                  where makeArc s d = FsmArcX s (transx xs) d

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = concatMap expandX

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- covert string to int
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- State encoding to Sn where n is reverse of encoding in b10
encToInt :: [Tristate] -> Int
encToInt enc = fromMaybe 0 ((readBin . concatMap show . reverse) enc)

fsmarcxToFsmarc :: FsmArcX a -> FsmArc a
fsmarcxToFsmarc arc = FsmArc newSourceEnc (transx arc) newDestEnc
    where newSourceEnc = (encToInt . srcEncx) arc
          newDestEnc = (encToInt . destEncx) arc

removeInvariants :: [FsmArcX a] -> [[Tristate]] -> [FsmArcX a]
removeInvariants xs inv = filter (\x -> not (destEncx x `elem` inv || srcEncx x `elem` inv)) xs

-- Produce all arcs with all X's resolved
createAllArcs :: Ord a => [([Transition a], Transition a)] -> [[Tristate]] -> [FsmArc a]
createAllArcs xs = map fsmarcxToFsmarc . removeInvariants (expandAllXs (createArcs xs))
