module Tuura.Concept.FSM.Translation where

import Data.List
import Data.List.Extra
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty, groupAllWith)
import Data.Ord
import qualified Data.Set as Set
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

import Tuura.Concept.FSM

import Tuura.Plato.Translate.Translation

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

-- Tristate used for findind before and after states from signal transitions.
-- The signals involved will be either TriTrue or TriFalse. All others are TriX.
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
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " "
                                        ++ show tran ++ " "
                                        ++ show tenc ++ ")\n"

-- Final FSM arc type using Ints for state encoding
data FsmArc a = FsmArc
    {
        srcEnc :: Int,
        trans :: Transition a,
        destEnc :: Int
    } deriving (Eq, Ord)

instance Show a => Show (FsmArc a) where
    show (FsmArc senc tran tenc) = "s" ++ show senc ++ " "
                                       ++ show tran ++ " s"
                                       ++ show tenc

-- Function to take a concept specification, and translate it to a FSM.
translateFSM :: (Show a, Ord a) => String -> String -> [a] -> (String -> IO ()) -> GHC.Interpreter()
translateFSM circuitName ctype signs out = do
    circ <- GHC.unsafeInterpret circuitName ctype
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype
             ++ ") -> CircuitConcept Signal"
    let circuit = apply circ
    let result = translate circuit signs
    if fst result
      then GHC.liftIO $ out (snd result)
      else GHC.liftIO $ putStrLn (snd result)

-- Function which performs the translation from concept specification to FSM
-- providing the FSM in .sg format. Will return errors if validation fails.
translate :: (Show a, Ord a) => CircuitConcept a -> [a] -> (Bool, String)
translate circuit signs =
    case validateInitialState signs circuit of
      Valid -> do
          let allCause = addConsistency (arcs circuit) signs
              groupByEffect = groupAllWith snd (arcLists allCause)
              sortedCause = concatMap handleArcs groupByEffect
              initState = getInitialState circuit signs
              allArcs = createAllArcs sortedCause
              reach = findReachables allArcs initState
              invarConcepts = invariant circuit
              invariants = concatMap (getInvariantStates signs) invarConcepts
              encodedInvs = map encToInt invariants
              reachableArcs = removeUnreachables allArcs reach
              inputSigns = filter ((==Input) . interface circuit) signs
              outputSigns = filter ((==Output) . interface circuit) signs
              internalSigns = filter ((==Internal) . interface circuit) signs
              reachInvs = filter (\i -> encToInt i `elem` reach) invariants
              allStates = [0..2 ^ length signs - 1]
              unreachables = (allStates \\ encodedInvs) \\ reach
          case validateFSM signs reachInvs (invariant circuit)
               <> validateInterface signs circuit of
              Valid -> do
                  let reachReport = genReachReport unreachables
                  (True, genFSM inputSigns outputSigns internalSigns
                         (map show reachableArcs) (show initState) reachReport)
              Invalid errs -> (False, addErrors errs)
      Invalid errs -> (False, addErrors errs)

-- Find the numeric value of the initial state.
getInitialState :: CircuitConcept a -> [a] -> Int
getInitialState circuit signs = encToInt state
  where
    state = map (fromBool  . getDefined . initial circuit) signs

-- From a boolean, return the Tristate version.
-- Used to find Tristate encodings of before and after states of signals.
fromBool :: Bool -> Tristate
fromBool x = if x then triTrue else triFalse

-- Create signal transition loops, ensuring a signal can transition both high
-- and low in the FSM. For each signal, add: rise ~> fall <> fall ~> rise.
addConsistency :: Ord a => [Causality (Transition a)] -> [a]
                        -> [Causality (Transition a)]
addConsistency allArcs signs = nubOrd (allArcs ++ consisArcs)
  where
    consisArcs = concatMap genConsis signs
    genConsis s = [Causality [rise s] (fall s), Causality [fall s] (rise s)]

-- Take causalities and apply the cartesian product to them. This combines them
-- so to create lists of required causes for each effect, which produce arcs.
handleArcs :: Ord a => NonEmpty ([Transition a], Transition a)
                    -> [([Transition a], Transition a)]
handleArcs xs = map (\m -> (m, effect)) transCauses
  where
    effect = snd (NonEmpty.head xs)
    effectCauses = NonEmpty.toList $ NonEmpty.map fst xs
    dnfCauses = simplifyDNF . convertCNFtoDNF . simplifyCNF $ CNF (map toLiteral effectCauses)
    transCauses = map toTransitions (fromDNF dnfCauses)

-- Check that no reachable states violate the invariant. This function attempts
-- to reach every state, checking whether it is in the invariant or not.
validateFSM :: Ord a => [a] -> [[Tristate]] -> [Invariant (Transition a)]
                     -> ValidationResult a
validateFSM signs reachInvs invs
    | null invVio = Valid
    | otherwise   = Invalid (map InvariantViolated invVio)
  where
    invVio = nubOrd (map fst check)
    check = concatMap (\i -> filter (\(_, x) -> i `elem` x) mapInvs) reachInvs
    mapInvs = map (\(NeverAll is) -> (is, invStates is)) invs
    invStates is = getInvariantStates signs (NeverAll is)

-- Function to generate the .sg format output from the translated FSM.
genFSM :: Show a => [a] -> [a] -> [a] -> [String] -> String -> String -> String
genFSM inputSigns outputSigns internalSigns arcStrs initState reachReport =
     printf tmpl (unwords ins)
                 (unwords outs)
                 (unwords ints)
                 (unlines arcStrs)
                 initState
                 reachReport
  where
    outs = map show outputSigns
    ins = map show inputSigns
    ints = map show internalSigns

-- Generate a report for the user about whether there are reachable states
-- which hold for the invariant.
genReachReport :: (Show a) => [a] -> String
genReachReport [] = "\ninvariant = reachability\n"
genReachReport es = "\nWarning:\n" ++
                    "The following state(s) hold for the invariant " ++
                    "but are not reachable:\n" ++
                    unlines unreachStates
  where
    unreachStates = [ "s" ++ show e | e <- es ]

-- Template for the .sg file. Each "%s" will be replaced by a string.
tmpl :: String
tmpl = unlines [".inputs %s",
                ".outputs %s",
                ".internal %s",
                ".state graph",
                "%s.marking {s%s}",
                ".end%s"]

-- Provide a list of all transitions in tuple, both cause and effect.
fullList :: ([a], a) -> [a]
fullList (l,t) = t:l

-- Provide a list of dont care transitions.
fullListm :: ([TransitionX a], Transition a) -> [TransitionX a]
fullListm (l,t) = toTransitionX t:l

-- Given [([a], b)], remove all b from a
removeDupes :: Eq a => [([Transition a], Transition a)]
                    -> [([Transition a], Transition a)]
removeDupes = map removeDupe1
  where
    removeDupe1 x = (filterDupes x, effect x)
    filterDupes x = filter ((/= effectSignal x) . signal) (causes x)
    effectSignal x = signal (effect x)
    effect = snd
    causes = fst

-- Convert transitions to TransitionX format.
toTransitionX :: Transition a -> TransitionX a
toTransitionX = liftM2 TransitionX signal (Tristate . Just . newValue)

-- Extract signal list from transition list
onlySignals :: [[Transition a]] -> [[a]]
onlySignals = map (map signal)

-- Find all signal names in design
getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = sort . foldl union [] . onlySignals

-- Signals which are not involved in a causality need a "dont care" or TriX
-- which this function provides.
addMissingSignals :: Ord a => [([Transition a], Transition a)] -> CausalityX a
addMissingSignals x = zip
                      (zipWith (++) newTransitions oldTransitions)
                      (map snd noDupes)
  where
    noDupes = removeDupes x
    oldTransitions = map (map toTransitionX . fst) noDupes
    newTransitions = ((map . map) (flip TransitionX triX) .
                     missingSignals . transitionList)
                     noDupes
    transitionList =  map fullList
    missingSignals y = map (getAllSignals y \\) (onlySignals y)

-- Take a list of transitions and convert it to a Tristate format containing
-- TriTrue, TriFalse and TriX (or don't care) states.
encode :: Ord a => [TransitionX a] -> [Tristate]
encode  = map mnewValue . sortTransitions
  where
    sortTransitions = sortBy (comparing msignal)

-- From the causalities, create arcs which contains previous and next states
-- in Tristate format, and the original transition.
createArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createArcs xs = zipWith3 createArc makeSrcEncs makeDestEncs activeTransitions
  where
    createArc senc tenc xTrans = FsmArcX senc xTrans tenc
    makeDestEncs = a xs
    makeSrcEncs = (a . map flipTransition) xs
    a = map (encode . fullListm) . addMissingSignals
    flipTransition x = (fst x, (invert . snd) x)
    invert = liftM2 Transition signal (not . newValue)
    activeTransitions = (map snd .  addMissingSignals) xs

-- Convert the invariant into a Tristate format, so we know what states should
-- never be reached.
getInvariantStates :: Ord a =>  [a] -> Invariant (Transition a) -> [[Tristate]]
getInvariantStates allSigns (NeverAll es) = expand (encode newTransitions)
  where
    newTransitions = transX ++ concatMap (map genTransX) missingSigns
    genTransX s = TransitionX { msignal = s, mnewValue = triX }
    transX = map toTransitionX es
    missingSigns = map (allSigns \\) (onlySignals [es]) -- TODO: Optimise
    expand t = case elemIndex triX t of
         Nothing -> [t]
         Just n  -> do
           let newTrue = replaceAtIndex triTrue t n
           let newFalse = replaceAtIndex triFalse t n
           expand newTrue ++ expand newFalse

-- Replace one element of a list at the specified index.
replaceAtIndex :: a -> [a] -> Int -> [a]
replaceAtIndex item ls n = a ++ (item:b)
  where (a, _:b) = splitAt n ls

-- Replace a TriX with both a 0 and 1, creating two new states.
expandX :: FsmArcX a -> [FsmArcX a]
expandX xs = case elemIndex triX (srcEncx xs) of
    Nothing -> [xs]
    Just n  -> do
      let newTrue = makeArc (replaceAtIndex triTrue (srcEncx xs) n)
                    (replaceAtIndex triTrue (destEncx xs) n)
      let newFalse = makeArc (replaceAtIndex triFalse (srcEncx xs) n)
                     (replaceAtIndex triFalse (destEncx xs) n)
      expandX newTrue ++ expandX newFalse
        where
          makeArc s = FsmArcX s (transx xs)

-- Replace all TriXs with a 0 and 1 in every source and destination encoding.
expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = concatMap expandX

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- covert string to int
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- State encoding to Sn where n is reverse of encoding in b10
-- Creates decimal values for state names using encoding.
encToInt :: [Tristate] -> Int
encToInt enc = fromMaybe 0 ((readBin . concatMap show . reverse) enc)

-- Convert an encodings containing no TriX to FsmArc format.
fsmarcxToFsmarc :: FsmArcX a -> FsmArc a
fsmarcxToFsmarc arc = FsmArc newSourceEnc (transx arc) newDestEnc
  where
    newSourceEnc = (encToInt . srcEncx) arc
    newDestEnc   = (encToInt . destEncx) arc

-- If some states are unreachable, remove them from the list to reduce the size
-- of the resulting FSM.
removeUnreachables :: [FsmArc a] -> [Int] -> [FsmArc a]
removeUnreachables xs reachables = filter (\s -> checkDest s && checkSrc s) xs
  where
    checkDest s = destEnc s `elem` reachables
    checkSrc s  = srcEnc s `elem` reachables

-- Produce all arcs with all X's resolved
createAllArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArc a]
createAllArcs = map fsmarcxToFsmarc . expandAllXs . createArcs

-- Visit each state that can be reached, and list them.
findReachables :: Ord a => [FsmArc a] -> Int -> [Int]
findReachables allArcs initState = nubOrd (visit initState allArcs Set.empty)

-- Visit a state, then visit it's children.
visit :: Ord a => Int -> [FsmArc a] -> Set.Set Int -> [Int]
visit state allArcs visited = [state] ++ concatMap
                                         (\s -> visit s allArcs newVisited)
                                         (Set.difference destStates visited)
  where
    arcSet = Set.fromList allArcs
    srcStates = Set.filter (\s -> srcEnc s == state) arcSet
    destStates = Set.map destEnc srcStates
    newVisited = Set.unions [Set.singleton state, visited, destStates]
