module Tuura.Plato.BoolToConcept.BooleanFunctions (
  fromFunctions) where

import Tuura.Parser.Boolean
import Data.List
import Data.List.Extra
import Data.Foldable
import Data.Maybe
import Data.Ord

type CNF a = [[Literal a]]

type DNF a = [[Literal a]]

data Literal a = Literal { variable :: a, polarity :: Bool } deriving (Eq, Ord)

fromFunctions :: String -> String -> (Bool, String)
fromFunctions setString resetString = do
    let setResult = parseExpr setString
    let resetResult = parseExpr resetString
    if (left setResult /= "")
      then (False, "parse error at " ++ left setResult)
    else if (left resetResult /= "")
      then (False, "parse error at " ++ left resetResult)
      else do
      let set = right setResult
      let reset = right resetResult
      let setVars = nub $ toList set
      let resetVars = nub $ toList reset
      let cnfSet = convertToCNF set setVars
      let cnfReset = convertToCNF reset resetVars
      let allVars = nub $ setVars ++ resetVars
      (True, createConceptSpec allVars cnfSet cnfReset)
      -- (True, "set:   " ++ (show cnfSet) ++ "\n" ++ "reset: " ++  (show cnfReset))
  where
    right (Right x) = x
    right (Left _) = right (parseExpr "")
    left  (Left x) = show x
    left _ = ""

convertToCNF :: Ord a => (Expr a) -> [a] -> (Expr a)
convertToCNF expr vars = ands $ map ors cnf
  where
    values = mapM (const [False, True]) vars
    fs = filter (\v -> not $ eval expr (\x -> getValue x vars v)) values
    genVar val var = Literal var val
    sim = simplifyCNF vars $ map (\v -> (map (\f -> (genVar (v !! f) (vars !! f))) [0..(length vars - 1)])) fs
    cnf = map (map (\s -> if (polarity s) then (Not (Var (variable s))) else (Var (variable s)))) sim

ors :: [Expr a] -> Expr a
ors x = SubExpr (foldl Or (head x) (tail x))

ands :: [Expr a] -> Expr a
ands x = foldl And (head x) (tail x)

createConceptSpec :: [String] -> (Expr String) -> (Expr String) -> String
createConceptSpec vars set reset = modName ++ imp
                                ++ circuit ++ topConcept ++ wh
                                ++ outRise ++ outFall
                                ++ inInter ++ outInter
                                ++ initState
    where
      modName    = "\nmodule Concept where \n\n"
      imp        = "import Tuura.Concept.STG \n\n"
      circuit    = "circuit " ++ unwords vars ++ " out = "
      topConcept = "outRise <> outFall <> interface <> initialState\n"
      wh         = "  where\n"
      rConcept   = intersperse "<>" $ map (genConcepts True) (listOrs set)
      outRise    = "    outRise = " ++ unwords rConcept
      fConcept   = intersperse "<>" $ map (genConcepts False) (listOrs reset)
      outFall    = "\n    outFall = " ++ unwords fConcept
      inputVars  = intersperse "," vars
      inInter    = "\n    interface = inputs [" ++ unwords inputVars ++ "]"
      outInter   = " <> outputs [out]"
      initState  = "\n    initialState = "
                ++ "initialise0 [" ++ unwords inputVars ++ " , out]"

genConcepts :: Bool -> [(Expr String)] -> String
genConcepts v e
    | (length e) == 1 = causes ++ op ++ effect
    | otherwise       = "[" ++ causes ++ "]" ++ op ++ effect
  where
    causes = unwords $ intersperse "," $ map (direction) e
    direction (Var a)       = "rise " ++ a
    direction (Not (Var a)) = "fall " ++ a
    direction _ = "error"
    op       = if (length e > 1) then " ~|~> " else " ~> "
    effect   = if (v) then "rise out" else "fall out"

listOrs :: (Expr String) -> [[(Expr String)]]
listOrs (And a b) = listOrs a ++ listOrs b
listOrs (Or a b) = [orVars (Or a b)]
listOrs (SubExpr a) = listOrs a
listOrs a = [[a]]

orVars :: (Expr String) -> [(Expr String)]
orVars (Or a b) = orVars a ++ orVars b
orVars (SubExpr a) = orVars a
orVars a = [a]

eval :: (Expr a) -> (a -> Bool) -> Bool
eval (Var a) f     = f a
eval (Not a) f     = not (eval a f)
eval (And a b) f   = eval a f && eval b f
eval (Or a b) f    = eval a f || eval b f
eval (SubExpr a) f = eval a f

getValue :: Eq a => a -> [a] -> [Bool] -> Bool
getValue var vars values = fromJust $ lookup var $ zip vars values

simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF x = removeRedundancies $ removeSupersets $
             map (sort . nub) sequenced
  where
    sequenced = sequence x

simplifyCNF :: Ord a => [a] -> CNF a -> CNF a
simplifyCNF vars c = sort $ removeSupersets $ removeCancels vars c

removeCancels :: Eq a => [a] -> CNF a -> CNF a
removeCancels [] whole = whole
removeCancels vars whole = removeCancels (tail vars) newWhole
  where
    var  = Literal (head vars) True
    nVar = Literal (head vars) False
    relevant = filter (var `elem`) whole
    nRelevant = map (replace [var] [nVar]) relevant
    existInWhole = filter (`elem` whole) nRelevant
    toBeRemoved = map (replace [nVar] [var]) existInWhole
    allToBeRemoved = existInWhole ++ toBeRemoved
    removed = whole \\ allToBeRemoved
    replacements = map (delete var) toBeRemoved
    newWhole = whole ++ replacements

convertCNFtoDNF :: Ord a => CNF a -> DNF a
convertCNFtoDNF l = map (sort . nub) (sequence l)

-- Sort list of lists from largest length to shortest, then remove any lists
-- that have shorter subsequences within the rest of the list.
removeSupersets :: Eq a => CNF a -> CNF a
removeSupersets s = [ x | (x:xs) <- tails sortByLength, not (check x xs) ]
  where
    check current = any (`isSubsequenceOf` current)
    sortByLength  = sortBy (comparing $ negate . length) s

removeRedundancies :: Eq a => CNF a -> CNF a
removeRedundancies = filter (\ts -> all (\t -> not ((neg t) `elem` ts)) ts)
  where
    neg x = Literal { variable = variable x, polarity = not $ polarity x}
