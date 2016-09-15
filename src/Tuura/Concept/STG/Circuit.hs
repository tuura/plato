module Tuura.Concept.STG.Circuit (
    State (..), Transition (..),
    rise, fall, toggle, oldValue, before, after,
    CircuitConcept,
    consistency, initialise,
    initialise0, initialise1,
    (~>), (~|~>),
    buffer, inverter, cElement, meElement,
    andGate, orGate, me, handshake,
    handshake00, handshake11, inputs,
    outputs, internals
    ) where

import Tuura.Concept.STG.Abstract
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

consistency :: CircuitConcept a
consistency = mempty

initialise :: Eq a => a -> Bool -> CircuitConcept a
initialise a v = initialConcept $ \s -> if s == a then Defined v else Undefined

initialise0 :: Eq a => [a] -> CircuitConcept a
initialise0 as = if (as /= []) then initialise (head as) False <> initialise0 (tail as) else mempty

initialise1 :: Eq a => [a] -> CircuitConcept a
initialise1 as = if (as /= []) then initialise (head as) True <> initialise1 (tail as) else mempty

(~>) :: Transition a -> Transition a -> CircuitConcept a
(~>) = arcConcept

(~|~>) :: [Transition a] -> Transition a -> CircuitConcept a
(~|~>) = orCausality

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
andGate a b c = rise a ~> rise c <> rise b ~> rise c <> [fall a, fall b] ~|~> fall c

orGate :: a -> a -> a -> CircuitConcept a
orGate a b c = [rise a, rise b] ~|~> rise c <> fall a ~> fall c <> fall b ~> fall c

-- Protocol-level concepts
handshake :: a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: Eq a => a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialise a False <> initialise b False

handshake11 :: Eq a => a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialise a True <> initialise b True

-- TODO: Restrict the initial state so that a=b=1 is not allowed.
me :: a -> a -> CircuitConcept a
me a b = fall a ~> rise b <> fall b ~> rise a

-- Signal type declaration concepts
inputs :: Eq a => [a] -> CircuitConcept a
inputs ins = interfaceConcept $ \s -> if s `elem` ins then Input else Unused

outputs :: Eq a => [a] -> CircuitConcept a
outputs outs = interfaceConcept $ \s -> if s `elem` outs then Output else Unused

internals :: Eq a => [a] -> CircuitConcept a
internals ints = interfaceConcept $ \s -> if s `elem` ints then Internal else Unused
