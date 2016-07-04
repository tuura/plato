{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}
module Tuura.PetriNet (
    Capacity, Weight, Tokens, Marking, PetriNet (..), presetP, postsetP,
    readsetP, presetT, postsetT, readsetT, mapP, mapT, mapPT, mapMarking,
    enabled, disabled, fire
    ) where

import Data.Bifunctor
import Data.Set
import Data.Map

type Capacity  = Int
type Weight    = Int
type Tokens    = Int
type Marking p = Map p Tokens

data PetriNet p t = PetriNet
    { places        :: Map p Capacity
    , transitions   :: Set t
    , producingArcs :: Map (p, t) Weight
    , consumingArcs :: Map (p, t) Weight
    , readArcs      :: Map (p, t) Weight }
    deriving Show

presetP, postsetP, readsetP :: Ord p => PetriNet p t -> p -> Map t Weight
presetP  net p = matchFst p $ producingArcs net
postsetP net p = matchFst p $ consumingArcs net
readsetP net p = matchFst p $ readArcs      net

presetT, postsetT, readsetT :: Ord t => PetriNet p t -> t -> Map p Weight
presetT  net t = matchSnd t $ consumingArcs net
postsetT net t = matchSnd t $ producingArcs net
readsetT net t = matchSnd t $ readArcs      net

matchFst :: Eq p => p -> Map (p, t) w -> Map t w
matchFst p = mapKeysMonotonic snd . filterWithKey (\(p', _) _ -> p == p')

matchSnd :: Eq t => t -> Map (p, t) w -> Map p w
matchSnd t = mapKeysMonotonic fst . filterWithKey (\(_, t') _ -> t == t')

mapPT :: (Ord q, Ord u) => (p -> q) -> (t -> u) -> PetriNet p t -> PetriNet q u
mapPT f g PetriNet {..} = PetriNet
    (mapKeys f places)
    (Data.Set.map g transitions)
    (mapKeys (bimap f g) producingArcs)
    (mapKeys (bimap f g) consumingArcs)
    (mapKeys (bimap f g) readArcs     )

mapP :: (Ord q, Ord t) => (p -> q) -> PetriNet p t -> PetriNet q t
mapP f = mapPT f id
mapT :: (Ord p, Ord u) => (t -> u) -> PetriNet p t -> PetriNet p u
mapT = mapPT id

mapMarking :: Ord q => (p -> q) -> Marking p -> Marking q
mapMarking = Data.Map.mapKeys

enabled :: (Ord p, Ord t) => PetriNet p t -> Marking p -> t -> Bool
enabled net m t = foldrWithKey (\p w -> (&& enough p w)) test (presetT net t)
  where
    test = foldrWithKey (\p w -> (&& enough p w)) True (readsetT net t)
    enough p w = findWithDefault 0 p m >= w

disabled :: (Ord p, Ord t) => PetriNet p t -> Marking p -> t -> Bool
disabled net m t = not $ enabled net m t

fire :: (Ord p, Ord t) => PetriNet p t -> t -> Marking p -> Marking p
fire net t m = foldrWithKey (\p w -> adjust (+w) p) m' (postsetT net t)
  where
    m' = foldrWithKey (\p w -> adjust (subtract w) p) m (presetT net t)
