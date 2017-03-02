module Tuura.Concept.Circuit.Basic (
    Interface (..),
    InitialValue (..),
    Invariant (..),
    Concept (..),
    initialConcept, arcConcept,
    orCausality, interfaceConcept,
    invariantConcept
    ) where

import Data.Monoid

-- Abstract concepts
-- * s is the type of states
-- * e is the type of events

data Interface = Unused | Input | Output | Internal deriving (Ord, Eq, Show)

instance Monoid Interface where
    mempty = Unused

    mappend = max

data InitialValue = Undefined | Defined { getDefined :: Bool } | Inconsistent deriving (Eq, Show)

instance Monoid InitialValue where
    mempty = Undefined

    mappend Inconsistent _ = Inconsistent
    mappend _ Inconsistent = Inconsistent
    mappend Undefined x = x
    mappend x Undefined = x
    mappend (Defined x) (Defined y) = if x == y then Defined x else Inconsistent

data Invariant e = NeverAll [e] deriving (Eq, Show)

-- Note, type parameter s is unused in this implementation and may be removed later.
data Concept s e a = Concept
                   {
                       initial   :: a -> InitialValue,
                       arcs      :: [([e], e)],
                       interface :: a -> Interface,
                       invariant :: [Invariant e]
                   }

instance Monoid (Concept s e a) where
    mempty = Concept mempty mempty mempty mempty

    mappend a b = Concept
                  {
                      initial   = initial a   <> initial b,
                      arcs      = arcs a      <> arcs b,
                      interface = interface a <> interface b,
                      invariant = invariant a <> invariant b
                  }

arcConcept :: e -> e -> Concept s e a
arcConcept from to = mempty { arcs = [([from], to)] }

orCausality :: [e] -> e -> Concept s e a
orCausality from to = mempty { arcs = [(from, to)] }

initialConcept :: (a -> InitialValue) -> Concept s e a
initialConcept f = mempty { initial = f }

interfaceConcept :: (a -> Interface) -> Concept s e a
interfaceConcept f = mempty { interface = f }

invariantConcept :: Invariant e -> Concept s e a
invariantConcept i = mempty { invariant = [i] }
