module Tuura.Plato.Translation where

import Data.Char
import Data.List.Extra

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

data ValidationResult a = Valid | Invalid [ValidationError a] deriving Eq

data ValidationError a = UnusedSignal a
                       | InconsistentInitialState a
                       | UndefinedInitialState a
                       | InvariantInitialState a
                       deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

addErrors :: (Eq a, Show a) => [ValidationError a] -> String
addErrors errs =
        (if (unused errs) /= []
        then "The following signals are not declared as input, output or internal: \n"
             ++ unlines (map show (unused errs)) ++ "\n"
        else "") ++
        (if (incons errs) /= []
        then "The following signals have inconsistent inital states: \n"
             ++ unlines (map show (incons errs)) ++ "\n"
        else "") ++
        (if (undefd errs) /= []
        then "The following signals have undefined initial states: \n"
             ++ unlines (map show (undefd errs)) ++ "\n"
        else "") ++
        (if (invart errs) /= []
        then "The invariant does not hold for the initial state.\nSignals with erroneous initial states: \n"
             ++ unlines (map show (invart errs)) ++ "\n"
        else "")
    where
        unused es = [ a | UnusedSignal a <- es ]
        incons es = [ a | InconsistentInitialState a <- es ]
        undefd es = [ a | UndefinedInitialState a <- es ]
        invart es = [ a | InvariantInitialState a <- es ]

validate :: Ord a => [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit
    | unused ++ inconsistent ++ undef ++ invInit == [] = Valid
    | otherwise = Invalid ((map UnusedSignal unused) ++ (map InconsistentInitialState inconsistent)
               ++ (map UndefinedInitialState undef) ++ (map InvariantInitialState invInit))
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs
    invInit      = concatMap (\i -> checkInitialStates (signs \\ inconsistent) i (initial circuit)) (invariant circuit)

checkInitialStates :: Ord a => [a] -> Invariant (Transition a) -> (a -> InitialValue) -> [a]
checkInitialStates signs (NeverAll es) initials = nubOrd (if (all (`elem` initialStates) es) then (invariantError es) else [])
  where
    initialStates = map (\s -> Transition { signal = s , newValue = getDefined $ initials s }) signs

invariantError :: [Transition a] -> [a]
invariantError = map signal

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct l = sequence l
