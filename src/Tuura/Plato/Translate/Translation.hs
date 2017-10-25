module Tuura.Plato.Translate.Translation where

import Data.Char
import Data.Monoid

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

data ValidationResult a = Valid | Invalid [ValidationError a] deriving Eq

instance Monoid (ValidationResult a) where
    mempty = mempty

    mappend Valid x = x
    mappend x Valid = x
    mappend (Invalid es) (Invalid fs) = Invalid (fs ++ es)

data ValidationError a = UnusedSignal a
                       | InconsistentInitialState a
                       | UndefinedInitialState a
                       | InvariantViolated [Transition a]
                       deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

-- Prepare output explaining errors to users.
-- TODO: Tidy up function, it looks ugly.
addErrors :: (Eq a, Show a) => [ValidationError a] -> String
addErrors errs = "Error\n" ++
        (if unused /= []
        then "The following signals are not declared as input, "
             ++ "output or internal: \n" ++ unlines (map show unused) ++ "\n"
        else "") ++
        (if incons /= []
        then "The following signals have inconsistent inital states: \n"
             ++ unlines (map show incons) ++ "\n"
        else "") ++
        (if undefd /= []
        then "The following signals have undefined initial states: \n"
             ++ unlines (map show undefd) ++ "\n"
        else "") ++
        (if invVio /= []
        then "The following state(s) are reachable " ++
             "but the invariant does not hold for them:\n" ++
             unlines (map show invVio) ++ "\n"
        else "")
    where
        unused = [ a | UnusedSignal a             <- errs ]
        incons = [ a | InconsistentInitialState a <- errs ]
        undefd = [ a | UndefinedInitialState a    <- errs ]
        invVio = [ a | InvariantViolated a        <- errs ]

-- Validate initial states and interface.
validate :: [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit = validateInitialState signs circuit
                      <> validateInterface signs circuit

-- Validate initial state - If there are any undefined or inconsistent
-- initial states, then these will populate the list.
validateInitialState :: [a] -> CircuitConcept a -> ValidationResult a
validateInitialState signs circuit
    | null (undef ++ inconsistent) = Valid
    | otherwise = Invalid (map UndefinedInitialState undef
                        ++ map InconsistentInitialState inconsistent)
  where
    undef        = filter ((==Undefined) . initial circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs

-- Validate interface - If there are any unused signals then these
-- will populate the list.
validateInterface :: [a] -> CircuitConcept a -> ValidationResult a
validateInterface signs circuit
    | null unused = Valid
    | otherwise   = Invalid (map UnusedSignal unused)
  where
    unused       = filter ((==Unused) . interface circuit) signs

toLiteral :: [Transition a] -> [Literal a]
toLiteral = map (\t -> Literal (signal t) (newValue t))

toTransitions :: [Literal a] -> [Transition a]
toTransitions = map (\l -> Transition (variable l) (polarity l))

--Create a tuple containing a list of possible causes, for each effect.
arcLists :: [Causality (Transition a)] -> [([Transition a], Transition a)]
arcLists xs = [ (f, t) | Causality f t <- xs ]

convert :: Enum a => CircuitConcept a -> CircuitConcept Signal
convert c = mempty
         {
           initial = convertFunction (initial c),
           arcs = fmap convertCausality (arcs c),
           interface = convertFunction (interface c),
           invariant = fmap convertInvariant (invariant c)
         }

convertFunction :: Enum a => (a -> b) -> (Signal -> b)
convertFunction f (Signal i) = f $ toEnum i

convertCausality :: Enum a => Causality (Transition a) -> Causality (Transition Signal)
convertCausality (Causality f t) = Causality (map convertTrans f) (convertTrans t)

convertInvariant :: Enum a => Invariant (Transition a) -> Invariant (Transition Signal)
convertInvariant (NeverAll es) = NeverAll (map convertTrans es)

convertTrans :: Enum a => Transition a -> Transition Signal
convertTrans t = Transition (Signal $ fromEnum $ signal t) (newValue t)
