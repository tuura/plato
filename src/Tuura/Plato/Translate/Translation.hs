-----------------------------------------------------------------------------
-- |
-- Module     : Tuura.Plato.Translate.Translation
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
-- This module defines several functions which are common to the translation
-- of Asynchronous Concepts to either STGs or State Graphs.
--
-----------------------------------------------------------------------------

module Tuura.Plato.Translate.Translation where

import Data.Char
import Data.Monoid

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

{- | 'ValidationResult' is a data type used to define whether the validation,
performed by several functions in this module, was successful or not.
'Valid' indicates that validation was succesful.
'Invalid' indicates that validation failed, and contains a list of the
errors which can then be reported back to the user.

The 'Monoid' instance is used to compose results. It contains some simple rules
for the compositions, i.e. If two 'ValidationResults' objects are 'Valid' then
the result of the composition is valid. If at least one is 'Invalid', then the
result must be 'Invalid'. If both are 'Invalid' then the result is 'Invalid'
and the list of errors is combined, to ensure that all errors are reported to
the user.
-}
data ValidationResult a = Valid | Invalid [ValidationError a] deriving Eq

instance Monoid (ValidationResult a) where
    mempty = mempty

    mappend Valid x = x
    mappend x Valid = x
    mappend (Invalid es) (Invalid fs) = Invalid (fs ++ es)

{-| 'ValidationError' is a type used to define the type of error found during
the validation performed by several functions in this module.

'UnusedSignal' occurs when a signal in the provided concept specification is
not provided with an interface, i.e. It is not declared as either an 'Input',
'Output' or 'Internal' signal. The affected signals are stored as part of this.

'InconsistentInitialState' occurs when a signal has its initial state defined
as both 'High' and 'Low'. The affected signals are stored as part of this.

'UndefinedInitialState' occurs when a signal has no initial state defined.
The affected signals are stored as part of this.

'InvariantViolated' occurs when a state can be reached which is defined,
using a 'never' concept, to not be part of the invariant. The transitions
which are part of the associated 'never' concept are stored as part of this.
-}
data ValidationError a = UnusedSignal a
                       | InconsistentInitialState a
                       | UndefinedInitialState a
                       | InvariantViolated [Transition a]
                       deriving Eq

{-| 'Signal' is a type which is used to refer to signals as defined in a
Concept specification. The number of signals in the specification is known
and a signal can be referenced by an integer.

The 'Show' instance converts the integer value of a signal to a letter for
reference. This follows the alphabet for the first 26 signals, and then will
be referred to as 'S' and the integer value after this.

'Ord' is used to compare the integer value of signals, to determine whether
two signals are infact the same signal.
-}
data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

-- TODO: Tidy up function, it looks ugly.
-- | Prepare output explaining errors found during validation to users.
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

-- | Validate initial states and interface.
validate :: [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit = validateInitialState signs circuit
                      <> validateInterface signs circuit

-- | Validate initial state - If there are any undefined or inconsistent
-- initial states, then these will populate the list.
validateInitialState :: [a] -> CircuitConcept a -> ValidationResult a
validateInitialState signs circuit
    | null (undef ++ inconsistent) = Valid
    | otherwise = Invalid (map UndefinedInitialState undef
                        ++ map InconsistentInitialState inconsistent)
  where
    undef        = filter ((==Undefined) . initial circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs

-- | Validate interface - If there are any unused signals then these
-- will populate the list.
validateInterface :: [a] -> CircuitConcept a -> ValidationResult a
validateInterface signs circuit
    | null unused = Valid
    | otherwise   = Invalid (map UnusedSignal unused)
  where
    unused       = filter ((==Unused) . interface circuit) signs

-- | Converts a 'Transition' to a 'Literal', a data type which is used by the
--Tuura Boolean library.
toLiteral :: [Transition a] -> [Literal a]
toLiteral = map (\t -> Literal (signal t) (newValue t))

-- | Converts a 'Literal' to 'Transitions'.
toTransitions :: [Literal a] -> [Transition a]
toTransitions = map (\l -> Transition (variable l) (polarity l))

-- | Converts 'Causality' concepts into a tuple, containing a list of possible
-- causes, for each effect.
arcLists :: [Causality (Transition a)] -> [([Transition a], Transition a)]
arcLists xs = [ (f, t) | Causality f t <- xs ]

-- | Converts from a 'CircuitConcept' using the polymorphic @a@ type, to a
-- CircuitConcept using the 'Signal' type.
convert :: Enum a => CircuitConcept a -> CircuitConcept Signal
convert c = mempty
         {
           initial = convertFunction (initial c),
           arcs = fmap convertCausality (arcs c),
           interface = convertFunction (interface c),
           invariant = fmap convertInvariant (invariant c)
         }

-- | For every function, such as the 'initial' function or 'interface'
-- function, Convert from using the polymorphic @a@ type to using the 'Signal'
-- type.
convertFunction :: Enum a => (a -> b) -> (Signal -> b)
convertFunction f (Signal i) = f $ toEnum i

-- | Converts all causalities from using the polymorphic @a@ type, to using the
-- 'Signal' type.
convertCausality :: Enum a => Causality (Transition a) -> Causality (Transition Signal)
convertCausality (Causality f t) = Causality (map convertTrans f) (convertTrans t)

-- | Converts all invariant concepts from using the polymorphic @a@ type, to
-- using the 'Signal' type.
convertInvariant :: Enum a => Invariant (Transition a) -> Invariant (Transition Signal)
convertInvariant (NeverAll es) = NeverAll (map convertTrans es)

-- | Converts an individual 'Transition' from using the polymorphic @a@ type,
-- to using the 'Signal' type.
convertTrans :: Enum a => Transition a -> Transition Signal
convertTrans t = Transition (Signal $ fromEnum $ signal t) (newValue t)
