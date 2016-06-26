module Tuura.Concept.Circuit (
    State (..), Transition (..),
    rise, fall, toggle, oldValue, before, after,
    CircuitConcept,
    consistency, initialise, causality, andCausalities, orCausalities,
    (~>), (~&~>), (~|~>),
    buffer, inverter, cElement, meElement, andGate, orGate,
    silent, me, handshake, handshake00, handshake11
    ) where

import Tuura.Concept.Abstract
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

-- Event-based concepts
consistency :: CircuitConcept a
consistency = excitedConcept before

initialise :: a -> Bool -> CircuitConcept a
initialise a = initialConcept . after . Transition a

never :: [(a, Bool)] -> CircuitConcept a
never = invariantConcept . foldr ((.||.) . notEqual) (const False)
  where
    notEqual (a, v) = before $ Transition a v

causality :: Eq a => Transition a -> Transition a -> CircuitConcept a
causality cause effect =
    excitedConcept $ \t -> if t == effect then after cause else const True

andCausalities :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
andCausalities causes effect =
    excitedConcept $ \t -> if t == effect
                           then foldr ((.&&.) . after) (const True) causes
                           else const True

orCausalities :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
orCausalities causes effect =
    excitedConcept $ \t -> if t == effect
                           then foldr ((.||.) . after) (const False) causes
                           else const True

(~>) :: Eq a => Transition a -> Transition a -> CircuitConcept a
(~>) = causality

(~&~>) :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
(~&~>) = andCausalities

(~|~>) :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
(~|~>) = orCausalities

silent :: Eq a => a -> CircuitConcept a
silent s = excitedConcept $ \e _ -> signal e /= s

-- Gate-level concepts
buffer :: Eq a => a -> a -> CircuitConcept a
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: Eq a => a -> a -> CircuitConcept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

cElement :: Eq a => a -> a -> a -> CircuitConcept a
cElement a b c = buffer a c <> buffer b c

meElement :: Eq a => a -> a -> a -> a -> CircuitConcept a
meElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> me g1 g2

andGate :: Eq a => a -> a -> a -> CircuitConcept a
andGate a b c = [rise a, rise b] ~&~> rise c <> [fall a, fall b] ~|~> fall c

orGate :: Eq a => a -> a -> a -> CircuitConcept a
orGate a b c = [rise a, rise b] ~|~> rise c <> [fall a, fall b] ~&~> fall c

-- Protocol-level concepts
handshake :: Eq a => a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: Eq a => a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialise a False <> initialise b False

handshake11 :: Eq a => a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialise a True <> initialise b True

me :: Eq a => a -> a -> CircuitConcept a
me a b = fall a ~> rise b <> fall b ~> rise a
      <> initialise a False <> initialise b False
      <> never [(a, True), (b, True)]
