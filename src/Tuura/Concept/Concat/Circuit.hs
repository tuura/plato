module Tuura.Concept.Concat.Circuit (
    State (..), Transition (..),
    rise, fall, toggle, oldValue, before, after,
    CircuitConcept,
    consistency, initialise,
    (~>),
    buffer, inverter, cElement, meElement,
    me, handshake, handshake00, handshake11
    ) where

import Tuura.Concept.Concat.Abstract
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
    deriving Eq

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

type CircuitConcept a = Concept (State a) (Transition a)

consistency :: CircuitConcept a
consistency = mempty

initialise :: a -> Bool -> CircuitConcept a
initialise a = initialConcept . after . Transition a

(~>) :: Transition a -> Transition a -> CircuitConcept a
(~>) = arcConcept

-- Gate-level concepts
buffer :: a -> a -> CircuitConcept a
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: a -> a -> CircuitConcept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

cElement :: a -> a -> a -> CircuitConcept a
cElement a b c = buffer a c <> buffer b c

meElement :: a -> a -> a -> a -> CircuitConcept a
meElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> me g1 g2

-- Protocol-level concepts
handshake :: a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialise a False <> initialise b False

handshake11 :: a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialise a True <> initialise b True

me :: a -> a -> CircuitConcept a
me a b = fall a ~> rise b <> fall b ~> rise a
      <> initialise a False <> initialise b False
