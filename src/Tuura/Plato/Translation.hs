module Tuura.Plato.Translation where

import Data.Char
import Data.List.Extra

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

data ValidationResult a = Valid | Invalid [a] [a] [a] [a] deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

addErrors :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a] -> String
addErrors unused incons undef invInit = un ++ ic ++ ud ++ iv
  where
    un = if (unused  /= []) then ("The following signals are not declared as input, output or internal: \n" ++ unlines (map show unused) ++ "\n") else ""
    ic = if (unused  /= []) then ("The following signals have inconsistent inital states: \n" ++ unlines (map show incons) ++ "\n") else ""
    ud = if (undef   /= []) then ("The following signals have undefined initial states: \n" ++ unlines (map show undef) ++ "\n") else ""
    iv = if (invInit /= []) then ("The initial state is an invariant state. Signals with erroneous initial states: \n" ++ unlines (map show invInit) ++ "\n") else ""

validate :: Ord a => [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit
    | unused ++ inconsistent ++ undef ++ invInit == [] = Valid
    | otherwise                             = Invalid unused inconsistent undef invInit
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs
    invInit      = checkInitialStates signs circuit

checkInitialStates :: Ord a => [a] -> CircuitConcept a -> [a]
checkInitialStates signs circuit = nubOrd (concatMap (\inv -> if (checkForInvariant inv initialStates) then (invariantError inv) else []) (invariant circuit))
  where
    initialStates = map (\s -> Transition { signal = s , newValue = getDefined $ initial circuit s }) signs

invariantError :: [Transition a] -> [a]
invariantError = map (signal)

checkForInvariant :: Eq a => [Transition a] -> [Transition a] -> Bool
checkForInvariant invs initialStates
    | invs == [] = True
    | otherwise  = ((head invs) `elem` initialStates) && (checkForInvariant (tail invs) initialStates)

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct l = sequence l
