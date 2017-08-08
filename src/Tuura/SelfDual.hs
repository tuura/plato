module Tuura.SelfDual (
    isSelfDual, getSelfDuals) where

import Control.Monad (replicateM)
import Data.List (sort)

import Tuura.Boolean

isSelfDual :: DNF String -> Bool
isSelfDual func = f == fd
    where f = final func
          fd = (final . dual) func
          dual = simplifyDNF . convertCNFtoDNF . dualDNF
          final = sort . fromDNF

-- List truth tables for all possible self-dual function of n variables
-- Descending order [2^n - 1 .. 0]
getSelfDuals :: Int -> [[Bool]]
getSelfDuals 0 = [[]]
getSelfDuals n = good
    where cells = 2^n
          halfCells = cells `div` 2
          possibles = replicateM cells [True,False]
          top = take halfCells
          bot = reverse . drop halfCells
          testMutex x = (not . or) (zipWith (&&) (top x) (bot x))
          good = [ x | x <- possibles, count x == halfCells, testMutex x]
          count = length . filter (==False)

parseToCNF :: String -> CNF String
parseToCNF = simplifyCNF . convertToCNF . right . parseExpr
    where right (Right x) = x
          right (Left _) = right (parseExpr "")

parseToDNF :: String -> DNF String
parseToDNF = simplifyDNF . convertCNFtoDNF . simplifyCNF . parseToCNF

dualDNF :: DNF a -> CNF a
dualDNF = CNF . fromDNF
