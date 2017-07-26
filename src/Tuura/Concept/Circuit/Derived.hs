module Tuura.Concept.Circuit.Derived (
    State (..), Transition (..),
    rise, fall, toggle, oldValue, before, after,
    CircuitConcept, dual, bubble,
    consistency, initialise,
    initialise0, initialise1,
    (~>), (~|~>), (~&~>),
    buffer, inverter, cElement, meElement,
    andGate, orGate, xorGate, me, never, handshake,
    handshake00, handshake11, inputs,
    outputs, internals
    ) where

import Tuura.Concept.Circuit.Basic
import Data.Monoid

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
bubbleInitialValue s f y = if (y == s) then bubbleVal (f y) else f y
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
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: a -> a -> CircuitConcept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

cElement :: a -> a -> a -> CircuitConcept a
cElement a b c = buffer a c <> buffer b c

meElement :: a -> a -> a -> a -> CircuitConcept a
meElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> me g1 g2

andGate :: a -> a -> a -> CircuitConcept a
andGate a b c = rise a ~> rise c <> rise b ~> rise c
             <> [fall a, fall b] ~|~> fall c

orGate :: a -> a -> a -> CircuitConcept a
orGate a b c = [rise a, rise b] ~|~> rise c
            <> fall a ~> fall c <> fall b ~> fall c

xorGate :: a -> a -> a -> CircuitConcept a
xorGate a b c = [rise a, rise b] ~|~> rise c <> [fall a, fall b] ~|~> rise c
             <> [rise a, fall b] ~|~> fall c <> [fall a, rise b] ~|~> fall c

-- Protocol-level concepts
handshake :: a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: Eq a => a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialise a False <> initialise b False

handshake11 :: Eq a => a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialise a True <> initialise b True

me :: a -> a -> CircuitConcept a
me a b = fall a ~> rise b <> fall b ~> rise a <> never [rise a, rise b]

never :: [Transition a] -> CircuitConcept a
never es = invariantConcept (NeverAll es)

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
