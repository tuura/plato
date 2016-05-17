{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
module Tuura.Concept.Circuit (
    State (..), Transition (..), wrap, CircuitConcept,
    rise, fall, toggle, oldValue, before, after,
    consistency, initialise, causality, andCausalities, orCausalities,
    (~>), (~&~>), (~|~>),
    buffer, inverter, cElement, meElement, andGate, orGate,
    silent, me, handshake, handshake00, handshake11
    ) where

import Tuura.Concept.Abstract
import Data.Monoid

-- Circuit primitives
-- Parameter a stands for the alphabet of signals
newtype State a = State { getState :: a -> Bool }

data Transition a = Transition
    {
        signal   :: a,
        newValue :: Bool -- Transition x True corresponds to x+
    }
    deriving (Eq, Functor)

instance Show a => Show (Transition a) where
    show (Transition s True ) = show s ++ "+"
    show (Transition s False) = show s ++ "-"

-- TODO: get rid of these instances
instance Bounded (State a) where
    minBound = State $ true
    maxBound = State $ false

instance (Enum a, Bounded a) => Show (State a) where
    show s =
        map (\a -> if getState s a then '1' else '0') [minBound..maxBound]

type CircuitConcept a = Concept (State a) (Transition a)

-- wrap is actuall invmap from Data.Functor.Invariant
wrap :: (a -> b) -> (b -> a) -> CircuitConcept a -> CircuitConcept b
wrap to from c = Concept
                 {
                     initial   = wrapSpace $ initial c,
                     excited   = \e -> wrapSpace $ excited c (fmap from e),
                     invariant = wrapSpace $ invariant c
                 }
  where
    wrapSpace p = \s -> p (State $ getState s . to)

rise :: a -> Transition a
rise a = Transition a True

fall :: a -> Transition a
fall a = Transition a False

toggle :: Transition a -> Transition a
toggle (Transition a v) = Transition a (not v)

oldValue :: Transition a -> Bool
oldValue (Transition _ v) = not v

before :: Transition a -> State a -> Bool
before t s = getState s (signal t) == oldValue t

after :: Transition a -> State a -> Bool
after t s = getState s (signal t) == newValue t

-- Event-based concepts
consistency :: CircuitConcept a
consistency = excitedConcept before

initialise :: Eq a => a -> Bool -> CircuitConcept a
initialise a = initialConcept . after . Transition a

never :: Eq a => [(a, Bool)] -> CircuitConcept a
never = invariantConcept . foldr (.||.) false . map notEqual
  where
    notEqual (a, v) = before $ Transition a v

causality :: Eq a => Transition a -> Transition a -> CircuitConcept a
causality cause effect =
    excitedConcept $ \t -> if t == effect then after cause else true

andCausalities :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
andCausalities causes effect =
    excitedConcept $ \t -> if t == effect
                           then foldr (.&&.) true (map after causes)
                           else false

orCausalities :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
orCausalities causes effect =
    excitedConcept $ \t -> if t == effect
                           then foldr (.||.) false (map after causes)
                           else true

(~>) :: Eq a => Transition a -> Transition a -> CircuitConcept a
(~>) = causality

(~&~>) :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
(~&~>) = andCausalities

(~|~>) :: Eq a => [Transition a] -> Transition a -> CircuitConcept a
(~|~>) = orCausalities

silent :: Eq a => a -> CircuitConcept a
silent s = excitedConcept $ \e -> const $ signal e /= s

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
