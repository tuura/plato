module Circuit (
    module Concept,
    State (..),
    Transition (..),
    rise, fall, toggle, oldValue, before, after
    ) where

import Concept

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
