module Tuura.Concept.Concat.Abstract (
    SignalType (..),
    Concept (..),
    initialConcept, arcConcept,
    signalTypeConcept,
    (.&&.), (.||.),
    ) where

import Control.Applicative

-- Abstract concepts
-- * s is the type of states
-- * e is the type of events

data SignalType = Unused | Input | Internal | Output deriving (Ord, Eq, Show)

data Concept s e a = Concept
                   {
                       initial :: s -> Bool,
                       arcs    :: [(e, e)],
                       signalType :: a -> SignalType
                   }

instance Monoid (Concept s e a) where
    mempty = Concept
             {
                 initial = const True,
                 arcs    = [],
                 signalType = const Unused
             }
    mappend a b = Concept
                  {
                      initial = initial a .&&. initial b,
                      arcs    = arcs a     ++  arcs b,
                      signalType = \s -> signalType a s `max` signalType b s
                  }

arcConcept :: e -> e -> Concept s e a
arcConcept from to = mempty { arcs = [(from, to)] }

initialConcept :: (s -> Bool) -> Concept s e a
initialConcept f = mempty { initial = f }

signalTypeConcept :: (a -> SignalType) -> Concept s e a
signalTypeConcept f = mempty {signalType = f}

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftA2 (||)
