module Tuura.Boolean (
  module Tuura.Boolean.Parser,
  CNF (..), DNF (..), Literal (..),
  convertToCNF, genConcepts,
  simplifyCNF, simplifyDNF, convertCNFtoDNF) where

import Data.List
import Data.List.Extra
import Data.Foldable
import Data.Maybe
import Data.Ord

import Tuura.Boolean.Parser
import Tuura.Concept.Circuit

newtype CNF a = CNF { fromCNF :: [[Literal a]] }

newtype DNF a = DNF { fromDNF :: [[Literal a]] }

data Literal a = Literal { variable :: a, polarity :: Bool } deriving (Eq, Ord, Show)

convertToCNF :: Eq a => Expr a -> CNF a
convertToCNF expr = cnf
  where
    vars = nub $ toList expr
    values = mapM (const [False, True]) vars
    fs = filter (not . eval expr . getValue vars) values
    cnf = CNF $ map (\v -> map (\f -> Literal (vars !! f) (not $ v !! f)) [0..(length vars - 1)]) fs
    getValue vs vals v = fromJust $ lookup v $ zip vs vals

-- Generates a concept based on the effect transition, rise/fall as appropriate
genConcepts :: Transition String -> [Literal String] -> String
genConcepts effect e
    | length e == 1 = causes ++ " ~> " ++ eff
    | otherwise       = "[" ++ causes ++ "]" ++ " ~|~> " ++ eff
  where
    causes = unwords $ intersperse "," $ map direction e
    direction (Literal a True)  = "rise " ++ a
    direction (Literal a False) = "fall " ++ a
    eff   = (if newValue effect then "rise " else "fall ") ++ signal effect

eval :: Expr a -> (a -> Bool) -> Bool
eval (Var a) f     = f a
eval (Not a) f     = not (eval a f)
eval (And a b) f   = eval a f && eval b f
eval (Or a b) f    = eval a f || eval b f
eval (SubExpr a) f = eval a f

invert :: Literal a -> Literal a
invert l = Literal (variable l) (not $ polarity l)

-- Take a DNF and remove any redundancies and supersets for a more compact form.
simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF x = DNF (removeRedundancies $ removeSupersets $ fromDNF x)

-- Take a CNF and remove any sets that also exist with one negated variable
-- then remove any supersets that still exist.
simplifyCNF :: Eq a => CNF a -> CNF a
simplifyCNF c = CNF (removeSupersets $ fromCNF $ removeCancels vars c)
  where
    vars = nub $ concatMap (map variable) (fromCNF c)

-- For each variable in the expression, get all subexpressions which contain it
-- then find any of these with the current variable negated, remove these from
-- the expression and add these subexpressions without the current variable.
removeCancels :: Eq a => [a] -> CNF a -> CNF a
removeCancels [] whole = whole
removeCancels (v:vs) whole = removeCancels vs newWhole
  where
    var = Literal v True
    nVar = Literal v False
    relevant = filter (var `elem`) (fromCNF whole)
    nRelevant = map (replace [var] [nVar]) relevant
    existInWhole = filter (`elem` fromCNF whole) nRelevant
    toBeRemoved = map (replace [nVar] [var]) existInWhole
    replacements = map (delete var) toBeRemoved
    newWhole = CNF (fromCNF whole ++ replacements)

-- Apply cartesian product to a CNF function to get DNF,
-- needed to produce STGs and FSMs.
convertCNFtoDNF :: Ord a => CNF a -> DNF a
convertCNFtoDNF l = DNF $ map (sort . nub) (sequence (fromCNF l))

-- Sort list of lists from largest length to shortest, then remove any lists
-- that have shorter subsequences within the rest of the list.
removeSupersets :: Eq a => [[Literal a]] -> [[Literal a]]
removeSupersets s = [ x | (x:xs) <- tails sortByLength, not (check x xs) ]
  where
    check current = any (`isSubsequenceOf` current)
    sortByLength  = sortBy (comparing $ negate . length) s

-- Redundancies are subexpressions where one of the variables exists in another
-- subexpression, but negated. Both of these can be removed from the expression.
removeRedundancies :: Eq a => [[Literal a]] -> [[Literal a]]
removeRedundancies = filter (\ts -> all (\t -> invert t `notElem` ts) ts)
