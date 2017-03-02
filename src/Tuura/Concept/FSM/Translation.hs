module Tuura.Concept.FSM.Translation where

import Data.List
import Data.List.Extra
import Data.Ord (comparing)
import qualified Data.Set as Set
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
    } deriving (Eq, Ord)

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
          let allCause = addConsistency (arcs circuit) signs
              sortedCause = concatMap handleArcs (groupSortOn snd allCause)
              initialState = getInitialState circuit signs
              allArcs = createAllArcs sortedCause
              reachables = findReachables allArcs initialState
              invariants = map encToInt (concatMap (\i -> getInvariantStates i signs) (invariant circuit))
              reachableArcs = removeUnreachables allArcs reachables
              inputSigns = filter ((==Input) . interface circuit) signs
              outputSigns = filter ((==Output) . interface circuit) signs
              internalSigns = filter ((==Internal) . interface circuit) signs
              reachableInvariants = filter (`elem` reachables) invariants
              unreachables = ([0..2^(length signs) - 1] \\ invariants) \\ reachables
          if (reachableInvariants /= [])
              then reachableInvariantStateError reachableInvariants
            else do
              let reachReport = genReachReport unreachables
              genFSM inputSigns outputSigns internalSigns (map show reachableArcs) (show initialState) reachReport
      Invalid errs ->
          "Error. \n" ++ addErrors errs

getInitialState :: CircuitConcept a -> [a] -> Int
getInitialState circuit signs = encToInt state
  where
    state = map (fromBool  . getDefined . initial circuit ) signs

fromBool :: Bool -> Tristate
fromBool x = if x then triTrue else triFalse

addConsistency :: Ord a => [([Transition a], Transition a)] -> [a] -> [([Transition a], Transition a)]
addConsistency allArcs signs = nubOrd (allArcs ++ concatMap (\s -> [([rise s], fall s), ([fall s], rise s)]) signs)

handleArcs :: [([Transition a], Transition a)] -> [([Transition a], Transition a)]
handleArcs arcLists = result
        where
            effect = snd (head arcLists)
            effectCauses = map fst arcLists
            transCauses = cartesianProduct effectCauses
            result = map (\m -> (m, effect)) transCauses

genFSM :: Show a => [a] -> [a] -> [a] -> [String] -> String -> String -> String
genFSM inputSigns outputSigns internalSigns arcStrs initialState reachReport =
     printf tmpl (unwords ins) (unwords outs) (unwords ints) (unlines arcStrs) initialState reachReport
    where
      outs = map show outputSigns
      ins = map show inputSigns
      ints = map show internalSigns

reachableInvariantStateError :: Show a => [a] -> String
reachableInvariantStateError es = "Error:\n" ++
                                  "The following state(s) are reachable but the invariant does not hold for them:\n" ++
                                  unlines (errStates)
    where
      errStates = [ "s" ++ show e | e <- es ]

genReachReport :: (Eq a, Show a) => [a] -> String
genReachReport es
        | es == []  = "# invariant = reachability\n"
        | otherwise = "# Warning:\n# The following state(s) hold for the invariant but are not reachable:\n" ++
                      unlines (unreachStates)
    where
      unreachStates = [ "# s" ++ show e | e <- es ]

tmpl :: String
tmpl = unlines [".inputs %s", ".outputs %s", ".internals %s", ".state graph", "%s.marking {s%s}", "%s.end"]

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

getInvariantStates :: Ord a => Invariant (Transition a) -> [a] -> [[Tristate]]
getInvariantStates (NeverAll es) allSigns = expand (encode newTransitions)
    where newTransitions = transX ++ (concatMap (map (\s -> TransitionX { msignal = s, mnewValue = triX })) missingSignals)
          transX = map toTransitionX es
          missingSignals = map (allSigns \\) (onlySignals [es]) -- TODO: Optimise
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

removeUnreachables :: [FsmArc a] -> [Int] -> [FsmArc a]
removeUnreachables xs reachables = filter (\s -> (destEnc s `elem` reachables && srcEnc s `elem` reachables)) xs

-- Produce all arcs with all X's resolved
createAllArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArc a]
createAllArcs = map fsmarcxToFsmarc . expandAllXs . createArcs

findReachables :: Ord a => [FsmArc a] -> Int -> [Int]
findReachables allArcs initialState = nubOrd (visit initialState allArcs Set.empty)

visit :: Ord a => Int -> [FsmArc a] -> Set.Set Int -> [Int]
visit state allArcs visited = [state] ++ concatMap (\s -> visit s allArcs newVisited) (Set.difference destStates visited)
    where
      arcSet = Set.fromList allArcs
      srcStates = Set.filter (\s -> (srcEnc s) == state) arcSet
      destStates = Set.map destEnc srcStates
      newVisited = Set.unions [Set.singleton state, visited, destStates]
