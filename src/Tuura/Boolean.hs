module Tuura.Boolean (
  module Tuura.Boolean.Parser,
  CNF (..), DNF (..), Literal (..),
  convertToCNF, genConcepts,
  simplifyCNF, simplifyDNF, convertCNFtoDNF) where

import Tuura.Boolean.Parser
import Data.List
import Data.List.Extra
import Data.Foldable
import Data.Maybe
import Data.Ord

newtype CNF a = CNF { fromCNF :: [[Literal a]] }

newtype DNF a = DNF { fromDNF :: [[Literal a]] }

data Literal a = Literal { variable :: a, polarity :: Bool } deriving (Eq, Ord)

convertToCNF :: Eq a => Expr a -> CNF a
convertToCNF expr = cnf
  where
    vars = toList expr
    values = mapM (const [False, True]) vars
    fs = filter (\v -> not $ eval expr (\x -> getValue x vars v)) values
    genVar val var = Literal var val
    sim = simplifyCNF $ CNF $ map (\v -> nub $ (map (\f -> (genVar (v !! f) (vars !! f))) [0..(length vars - 1)])) fs
    cnf = CNF $ map (map (\s -> Literal (variable s) (not $ polarity s))) (fromCNF sim)

genConcepts :: Bool -> [Literal String] -> String
genConcepts v e
    | (length e) == 1 = causes ++ op ++ effect
    | otherwise       = "[" ++ causes ++ "]" ++ op ++ effect
  where
    causes = unwords $ intersperse "," $ map (direction) e
    direction (Literal a True)  = "rise " ++ a
    direction (Literal a False) = "fall " ++ a
    op       = if (length e > 1) then " ~|~> " else " ~> "
    effect   = if (v) then "rise out" else "fall out"

eval :: (Expr a) -> (a -> Bool) -> Bool
eval (Var a) f     = f a
eval (Not a) f     = not (eval a f)
eval (And a b) f   = eval a f && eval b f
eval (Or a b) f    = eval a f || eval b f
eval (SubExpr a) f = eval a f

getValue :: Eq a => a -> [a] -> [Bool] -> Bool
getValue var vars values = fromJust $ lookup var $ zip vars values

simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF x = DNF (removeRedundancies $ removeSupersets $ fromDNF x)

simplifyCNF :: Eq a => CNF a -> CNF a
simplifyCNF c = CNF (removeSupersets $ fromCNF $ removeCancels vars c)
  where
    vars = nub $ concatMap (map variable) (fromCNF c)

removeCancels :: Eq a => [a] -> CNF a -> CNF a
removeCancels [] whole = whole
removeCancels (v:vs) whole = removeCancels vs newWhole
  where
    var = Literal v True
    nVar = Literal v False
    relevant = filter (var `elem`) (fromCNF whole)
    nRelevant = map (replace [var] [nVar]) relevant
    existInWhole = filter (`elem` (fromCNF whole)) nRelevant
    toBeRemoved = map (replace [nVar] [var]) existInWhole
    replacements = map (delete var) toBeRemoved
    newWhole = CNF (fromCNF whole ++ replacements)

convertCNFtoDNF :: Ord a => CNF a -> DNF a
convertCNFtoDNF l = DNF $ map (sort . nub) (sequence (fromCNF l))

-- Sort list of lists from largest length to shortest, then remove any lists
-- that have shorter subsequences within the rest of the list.
removeSupersets :: Eq a => [[Literal a]] -> [[Literal a]]
removeSupersets s = [ x | (x:xs) <- tails sortByLength, not (check x xs) ]
  where
    check current = any (`isSubsequenceOf` current)
    sortByLength  = sortBy (comparing $ negate . length) s

removeRedundancies :: Eq a => [[Literal a]] -> [[Literal a]]
removeRedundancies = filter (\ts -> all (\t -> not ((neg t) `elem` ts)) ts)
  where
    neg x = Literal { variable = variable x, polarity = not $ polarity x}
