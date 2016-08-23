module Tuura.Concept.Concat.Abstract (
    Interface (..),
    Concept (..),
    initialConcept, arcConcept,
    interfaceConcept,
    (.&&.), (.||.),
    ) where

import Control.Applicative

-- Abstract concepts
-- * s is the type of states
-- * e is the type of events

data Interface = Unused | Input | Internal | Output deriving (Ord, Eq, Show)

data Concept s e a = Concept
                   {
                       initial :: s -> Bool,
                       arcs    :: [(e, e)],
                       interface :: a -> Interface
                   }

instance Monoid (Concept s e a) where
    mempty = Concept
             {
                 initial = const True,
                 arcs    = [],
                 interface = const Unused
             }
    mappend a b = Concept
                  {
                      initial = initial a .&&. initial b,
                      arcs    = arcs a     ++  arcs b,
                      interface = \s -> interface a s `max` interface b s
                  }

arcConcept :: e -> e -> Concept s e a
arcConcept from to = mempty { arcs = [(from, to)] }

initialConcept :: (s -> Bool) -> Concept s e a
initialConcept f = mempty { initial = f }

interfaceConcept :: (a -> Interface) -> Concept s e a
interfaceConcept f = mempty {interface = f}

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftA2 (||)
