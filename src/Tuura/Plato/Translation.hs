module Tuura.Plato.Translation where

import Data.Char
import Data.List.Extra

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

data ValidationResult a = Valid | Invalid [ValidationError a] deriving Eq

data ValidationError a = UnusedSignal [a]
                       | InconsistentInitialState [a]
                       | UndefinedInitialState [a]
                       | InvariantInitialState [a]
                       deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

addErrors :: Show a => ValidationError a -> String
addErrors err = case err of
        UnusedSignal as ->
            "The following signals are not declared as input, output or internal: \n" ++ unlines (signs as) ++ "\n"
        InconsistentInitialState as ->
            "The following signals have inconsistent inital states: \n" ++ unlines (signs as) ++ "\n"
        UndefinedInitialState as ->
            "The following signals have undefined initial states: \n" ++ unlines (signs as) ++ "\n"
        InvariantInitialState as ->
            "The initial state is an invariant state.\nSignals with erroneous initial states: \n" ++ unlines (signs as) ++ "\n"
    where
        signs = map show

  --   un ++ ic ++ ud ++ iv
  -- where
  --   un = if (unused  /= []) then ("The following signals are not declared as input, output or internal: \n" ++ unlines (map show unused) ++ "\n") else ""
  --   ic = if (unused  /= []) then ("The following signals have inconsistent inital states: \n" ++ unlines (map show incons) ++ "\n") else ""
  --   ud = if (undef   /= []) then ("The following signals have undefined initial states: \n" ++ unlines (map show undef) ++ "\n") else ""
  --   iv = if (invInit /= []) then ("The initial state is an invariant state.\nSignals with erroneous initial states: \n" ++ unlines (map show invInit) ++ "\n") else ""

validate :: Ord a => [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit
    | unused ++ inconsistent ++ undef ++ invInit == [] = Valid
    | otherwise = Invalid (
        (if unused       /= [] then [UnusedSignal unused]                   else []) ++
        (if inconsistent /= [] then [InconsistentInitialState inconsistent] else []) ++
        (if undef        /= [] then [UndefinedInitialState undef]           else []) ++
        (if invInit      /= [] then [InvariantInitialState invInit]         else []))
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs
    invInit      = concatMap (\i -> checkInitialStates signs i (initial circuit)) (invariant circuit)

checkInitialStates :: Ord a => [a] -> Invariant (Transition a) -> (a -> InitialValue) -> [a]
checkInitialStates signs (NeverAll es) initials = nubOrd (if (checkForInvariant es initialStates) then (invariantError es) else [])
  where
    initialStates = map (\s -> Transition { signal = s , newValue = getDefined $ initials s }) signs

invariantError :: [Transition a] -> [a]
invariantError = map signal

checkForInvariant :: Eq a => [Transition a] -> [Transition a] -> Bool
checkForInvariant invs initialStates = all (`elem` initialStates) invs

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct l = sequence l
