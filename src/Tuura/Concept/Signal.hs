{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Tuura.Concept.Signal (
    SignalTag (..), Signal (..), Input, Output, Internal, untagSignal,
    DependencyType (..), Transition (..), rise, fall, toggle, oldValue, pretty
    ) where

-- TODO: Do we need to export this?
-- Internal (I), output (O) and internal (T) signal tags
data SignalTag = I | O | T

-- Constructors for tagged signals
data Signal t a where
    Input    :: a -> Signal I a
    Output   :: a -> Signal O a
    Internal :: a -> Signal T a

-- Extracting a signal name by erasing the tag information
untagSignal :: Signal t a -> a
untagSignal (Input    a) = a
untagSignal (Output   a) = a
untagSignal (Internal a) = a

instance Eq a => Eq (Signal t a) where
    x == y = untagSignal x == untagSignal y

instance Show a => Show (Signal t a) where
    show (Input    a) = "Input "    ++ show a
    show (Output   a) = "Output "   ++ show a
    show (Internal a) = "Internal " ++ show a

-- Convenient type synonyms
type Input    = Signal I
type Output   = Signal O
type Internal = Signal T

-- TODO: Is this list complete/accurate?
-- Different types of causality-like relations
data DependencyType = Causality
                    | TimingAssumption
                    | ConcurrencyReduction
                    | Telepathy

type family Dependency (a :: SignalTag) (b :: SignalTag) where
    Dependency I I = TimingAssumption
    Dependency O O = ConcurrencyReduction
    Dependency T I = Telepathy
    Dependency a b = Causality

-- Signal transitions
data Transition t a = Transition
    { signal   :: Signal t a
    , newValue :: Bool -- Transition x True corresponds to x+
    } deriving Eq

rise :: Signal t a -> Transition t a
rise a = Transition a True

fall :: Signal t a -> Transition t a
fall a = Transition a False

toggle :: Transition t a -> Transition t a
toggle (Transition a v) = Transition a (not v)

oldValue :: Transition t a -> Bool
oldValue (Transition _ v) = not v

-- Pretty-printing transitions
pretty :: Show a => Transition t a -> String
pretty t = show (untagSignal $ signal t) ++ if newValue t then "+" else "-"
