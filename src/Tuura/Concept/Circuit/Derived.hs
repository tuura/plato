module Tuura.Concept.Circuit.Derived (
    module Tuura.Boolean,
    State (..), Transition (..),
    rise, fall, toggle, oldValue, before, after,
    CircuitConcept, dual, bubble,
    consistency, initialise,
    initialise0, initialise1,
    (~>), (~|~>), (~&~>),
    buffer, inverter, cElement, mutexElement,
    andGate, orGate, xorGate, mutex, never, handshake,
    handshake00, handshake11,
    cElementN, orGateN, andGateN,
    inputs, outputs, internals, function, complexGate
    ) where

import Tuura.Concept.Circuit.Basic
import Data.Monoid
import Tuura.Boolean

-- Circuit primitives
-- Parameter a stands for the alphabet of signals
newtype State a = State (a -> Bool)

instance Bounded (State a) where
    minBound = State $ const False
    maxBound = State $ const True

instance (Enum a, Bounded a) => Show (State a) where
    show (State value) =
        map (\a -> if value a then '1' else '0') [minBound..maxBound]

data Transition a = Transition
    {
        signal   :: a,
        newValue :: Bool -- Transition x True corresponds to x+
    }
    deriving (Eq, Ord)

instance Show a => Show (Transition a) where
    show (Transition s True ) = show s ++ "+"
    show (Transition s False) = show s ++ "-"

rise :: a -> Transition a
rise a = Transition a True

fall :: a -> Transition a
fall a = Transition a False

toggle :: Transition a -> Transition a
toggle (Transition a v) = Transition a (not v)

oldValue :: Transition a -> Bool
oldValue (Transition _ v) = not v

before :: Transition a -> State a -> Bool
before t (State value) = value (signal t) == oldValue t

after :: Transition a -> State a -> Bool
after t (State value) = value (signal t) == newValue t

type CircuitConcept a = Concept (State a) (Transition a) a

-- Concept transformations

-- Create causalities with all opposite transition direction to
-- the given specification, for all possible causes and effects.
dualCausality :: Causality (Transition a) -> Causality (Transition a)
dualCausality (Causality f t) = Causality (map toggle f) (toggle t)

-- Give the opposite initial states to the given specification.
dualInitialValue :: InitialValue -> InitialValue
dualInitialValue (Defined v) = Defined (not v)
dualInitialValue x = x

-- Negate the given invariant, to provide the opposite never states.
dualInvariant :: Invariant (Transition e) -> Invariant (Transition e)
dualInvariant (NeverAll es) = NeverAll (map toggle es)

-- Provide the dual of a specification, with every causality, initial state
-- and invariant being opposite to the given.
dual :: CircuitConcept a -> CircuitConcept a
dual c = mempty
         {
           initial = fmap dualInitialValue (initial c),
           arcs = fmap dualCausality (arcs c),
           interface = interface c,
           invariant = fmap dualInvariant (invariant c)
         }

-- Give the opposite initial state for the chosen signal
bubbleInitialValue :: Eq a => a -> (a -> InitialValue) -> (a -> InitialValue)
bubbleInitialValue s f y = if y == s then bubbleVal (f y) else f y
  where
    bubbleVal (Defined v) = Defined (not v)
    bubbleVal x = x

-- Perform the inversion for just transitions of the given signal
toggleSpecific :: Eq a => a -> Transition a -> Transition a
toggleSpecific a t
    | signal t == a = toggle t
    | otherwise     = t

-- For the selected signal, toggle the directions of all transitions for this.
bubbleCausality :: Eq a => a -> Causality (Transition a)
                        -> Causality (Transition a)
bubbleCausality s (Causality f t)
    | signal t == s = Causality (map (toggleSpecific s) f) (toggle t)
    | otherwise     = Causality (map (toggleSpecific s) f) t

-- Invert the invariant transition of the selected signal.
bubbleInvariant :: Eq a => a -> Invariant (Transition a)
                        -> Invariant (Transition a)
bubbleInvariant s (NeverAll es) = NeverAll (map (toggleSpecific s) es)

bubble :: Eq a => a -> CircuitConcept a -> CircuitConcept a
bubble s c = mempty
             {
                initial = bubbleInitialValue s (initial c),
                arcs = fmap (bubbleCausality s) (arcs c),
                interface = interface c,
                invariant = fmap (bubbleInvariant s) (invariant c)
             }

-- Signal-level concepts

consistency :: CircuitConcept a
consistency = mempty

initialise :: Eq a => a -> Bool -> CircuitConcept a
initialise a v = initialConcept $ \s -> if s == a then Defined v else Undefined

initialise0 :: Eq a => [a] -> CircuitConcept a
initialise0 [] = mempty
initialise0 as = initialise (head as) False <> initialise0 (tail as)

initialise1 :: Eq a => [a] -> CircuitConcept a
initialise1 [] = mempty
initialise1 as = initialise (head as) True <> initialise1 (tail as)

(~>) :: Transition a -> Transition a -> CircuitConcept a
(~>) = arcConcept

(~|~>) :: [Transition a] -> Transition a -> CircuitConcept a
(~|~>) = orCausality

(~&~>) :: [Transition a] -> Transition a -> CircuitConcept a
(~&~>) = andCausality

-- Gate-level concepts
buffer :: a -> a -> CircuitConcept a
buffer a z = rise a ~> rise z <> fall a ~> fall z

inverter :: a -> a -> CircuitConcept a
inverter a z = rise a ~> fall z <> fall a ~> rise z

cElement :: a -> a -> a -> CircuitConcept a
cElement a b z = buffer a z <> buffer b z

mutexElement :: a -> a -> a -> a -> CircuitConcept a
mutexElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> mutex g1 g2

orGate :: a -> a -> a -> CircuitConcept a
orGate a b z = [rise a, rise b] ~|~> rise z
            <> [fall a, fall b] ~&~> fall z

andGate :: a -> a -> a -> CircuitConcept a
andGate a b z = dual $ orGate a b z

xorGate :: a -> a -> a -> CircuitConcept a
xorGate a b z = [rise a, rise b] ~|~> rise z <> [fall a, fall b] ~|~> rise z
             <> [rise a, fall b] ~|~> fall z <> [fall a, rise b] ~|~> fall z

-- Protocol-level concepts
handshake :: a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: Eq a => a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialise a False <> initialise b False

handshake11 :: Eq a => a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialise a True <> initialise b True

mutex :: a -> a -> CircuitConcept a
mutex a b = fall a ~> rise b <> fall b ~> rise a <> never [rise a, rise b]

never :: [Transition a] -> CircuitConcept a
never es = invariantConcept (NeverAll es)

-- Generalized multi-input gates
cElementN :: [a] -> a -> CircuitConcept a
cElementN ins out = mconcat $ map (`buffer` out) ins

orGateN :: [a] -> a -> CircuitConcept a
orGateN ins out = map rise ins ~|~> rise out <> map fall ins ~&~> fall out

andGateN :: [a] -> a -> CircuitConcept a
andGateN ins out = dual $ orGateN ins out

-- Signal type declaration concepts
inputs :: Eq a => [a] -> CircuitConcept a
inputs ins = interfaceConcept $ \s ->
             if s `elem` ins
             then Input
             else Unused

outputs :: Eq a => [a] -> CircuitConcept a
outputs outs = interfaceConcept $ \s ->
               if s `elem` outs
               then Output
               else Unused

internals :: Eq a => [a] -> CircuitConcept a
internals ints = interfaceConcept $ \s ->
                 if s `elem` ints
                 then Internal
                 else Unused

function :: Eq a => Expr a -> Transition a -> CircuitConcept a
function cause effect = mconcat $ map (toConcept) (toTransitions cnf)
  where
    toConcept c = c ~|~> effect
    cnf = fromCNF $ simplifyCNF $ convertToCNF cause
    toTransitions = map (map (\l -> Transition (variable l) (polarity l)))

complexGate :: Eq a => Expr a -> Expr a -> a -> CircuitConcept a
complexGate set reset sig = function set (rise sig)
                         <> function reset (fall sig)
