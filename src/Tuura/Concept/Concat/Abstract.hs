module Tuura.Concept.Concat.Abstract (
    Concept (..),
    initialConcept, arcConcept,
    (.&&.), (.||.),
    ) where

import Control.Applicative

-- Abstract concepts
-- * s is the type of states
-- * e is the type of events
data Concept s e = Concept
                   {
                       initial :: s -> Bool,
                       arcs    :: [(e, e)]
                   }

instance Monoid (Concept s e) where
    mempty = Concept
             {
                 initial = const True,
                 arcs    = []
             }
    mappend a b = Concept
                  {
                      initial = initial a .&&. initial b,
                      arcs    = arcs a     ++  arcs b
                  }

arcConcept :: e -> e -> Concept s e
arcConcept from to = mempty { arcs = [(from, to)] }

initialConcept :: (s -> Bool) -> Concept s e
initialConcept f = mempty { initial = f }

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftA2 (||)
