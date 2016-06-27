{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}
module Tuura.PetriNet (
    Capacity, Weight, PetriNet (..), Preset (..), Postset (..), Readset (..),
    Tokens, Marking, Fire (..), Enabled (..), disabled,
    mapPlaces, mapTransitions, mapMarking
    ) where

import Data.Bifunctor
import Data.Set
import Data.Map

type Capacity = Int
type Weight   = Int

data PetriNet p t = PetriNet
    { places        :: Map p Capacity
    , transitions   :: Set t
    , producingArcs :: Map (p, t) Weight
    , consumingArcs :: Map (p, t) Weight
    , readArcs      :: Map (p, t) Weight }

class Preset m p t a b where
    preset :: a -> m p t -> Map b Weight

instance Ord p => Preset PetriNet p t p t where
    preset p = mapKeysMonotonic snd
             . filterWithKey (\(p', _) _ -> p == p') . producingArcs

instance Ord t => Preset PetriNet p t t p where
    preset t = mapKeysMonotonic fst
             . filterWithKey (\(_, t') _ -> t == t') . consumingArcs

class Postset m p t a b where
    postset :: a -> m p t -> Map b Weight

instance Ord p => Postset PetriNet p t p t where
    postset p = mapKeysMonotonic snd
              . filterWithKey (\(p', _) _ -> p == p') . consumingArcs

instance Ord t => Postset PetriNet p t t p where
    postset t = mapKeysMonotonic fst
              . filterWithKey (\(_, t') _ -> t == t') . producingArcs

class Readset m p t a b where
    readset :: a -> m p t -> Map b Weight

instance Ord p => Readset PetriNet p t p t where
    readset p = mapKeysMonotonic snd
              . filterWithKey (\(p', _) _ -> p == p') . readArcs

instance Ord t => Readset PetriNet p t t p where
    readset t = mapKeysMonotonic fst
              . filterWithKey (\(_, t') _ -> t == t') . readArcs

mapPlaces :: (Ord p', Ord t) => (p -> p') -> PetriNet p t -> PetriNet p' t
mapPlaces f PetriNet {..} = PetriNet
    (mapKeys f places)
    transitions
    (mapKeys (first f) producingArcs)
    (mapKeys (first f) consumingArcs)
    (mapKeys (first f) readArcs     )

mapTransitions :: (Ord p, Ord t') => (t -> t') -> PetriNet p t -> PetriNet p t'
mapTransitions f PetriNet {..} = PetriNet
    places
    (Data.Set.map f transitions)
    (mapKeys (second f) producingArcs)
    (mapKeys (second f) consumingArcs)
    (mapKeys (second f) readArcs     )

type Tokens    = Int
type Marking p = Map p Tokens

mapMarking :: Ord p' => (p -> p') -> Marking p -> Marking p'
mapMarking = Data.Map.mapKeys

class Enabled m p t where
    enabled :: t -> m p t -> Marking p -> Bool

instance (Ord p, Ord t) => Enabled PetriNet p t where
    enabled t net m = foldrWithKey (\p k -> (&& enough p k)) True (preset t net)
      where
        enough p k = findWithDefault 0 p m >= k

disabled :: Enabled m p t => t -> m p t -> Marking p -> Bool
disabled t net m = not $ enabled t net m

class Fire m p t where
    fire :: t -> m p t -> Marking p -> Marking p

instance (Ord p, Ord t) => Fire PetriNet p t where
    fire t net m = foldrWithKey (\p k -> adjust (+k) p) m' (postset t net)
      where
        m' = foldrWithKey (\p k -> adjust (subtract k) p) m (preset t net)
