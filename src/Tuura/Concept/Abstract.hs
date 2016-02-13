{-# LANGUAGE TypeFamilies #-}
module Tuura.Concept.Abstract (
    Concept (..),
    initialConcept, excitedConcept, invariantConcept,
    (.&&.), (.||.),
    quiescent
    ) where

import Control.Applicative

-- Abstract concepts
-- * s is the type of states
-- * e is the type of events
data Concept s e = Concept
                   {
                       initial   :: s -> Bool,
                       excited   :: e -> s -> Bool,
                       invariant :: s -> Bool
                   }

-- Concepts form a monoid:
-- * the empty concept permits everything
-- * two concepts are combined by AND-ing all predicates
instance Monoid (Concept s e) where
    mempty = Concept
             {
                 initial   = const True,
                 excited   = const $ const True,
                 invariant = const True
             }
    mappend a b = Concept
                  {
                      initial   =       initial a   .&&. initial b,
                      excited   = \e -> excited a e .&&. excited b e,
                      invariant =       invariant a .&&. invariant b
                  }

excitedConcept :: (e -> s -> Bool) -> Concept s e
excitedConcept f = mempty { excited = f }

initialConcept :: (s -> Bool) -> Concept s e
initialConcept f = mempty { initial = f }

invariantConcept :: (s -> Bool) -> Concept s e
invariantConcept f = mempty { invariant = f }

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftA2 (||)

quiescent :: Concept s e -> e -> s -> Bool
quiescent c e = not . excited c e
