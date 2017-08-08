module Tuura.SelfDual (
    isSelfDual, getSelfduals) where

import Data.List
import Tuura.Boolean

isSelfDual :: String -> Bool
isSelfDual func = f == fd
    where f = final dnf
          fd = (final . dual) dnf
          dnf = parseToDNF func
          dual = simplifyDNF . convertCNFtoDNF . dualDNF
          final = sort . fromDNF

-- List truth tables for all possible self-dual function of n variables
-- Descending order [2^n - 1 .. 0]
getSelfduals :: Int -> [[Bool]]
getSelfduals 0 = [[]]
getSelfduals n = good
    where cells = 2^n
          halfCells = cells `div` 2
          possibles = map (toBool cells) [0..2^cells - 1]
          top = take halfCells
          bot = reverse . drop halfCells
          testMutex x = (not . or) (zipWith (&&) (top x) (bot x))
          good = filter (\x -> (count x == halfCells) && testMutex x) possibles
          count = length . filter (==False)

parseToCNF :: String -> CNF String
parseToCNF = simplifyCNF . convertToCNF . right . parseExpr
    where right (Right x) = x
          right (Left _) = right (parseExpr "")

parseToDNF :: String -> DNF String
parseToDNF = simplifyDNF . convertCNFtoDNF . simplifyCNF . parseToCNF

dualDNF :: DNF a -> CNF a
dualDNF = CNF . fromDNF

showBool :: Bool -> Char
showBool True = '1'
showBool False = '0'

toBool :: Int -> Int -> [Bool]
toBool n x = replicate (n - length l) False ++ l
    where l = (map intToBool . toBin) x

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (helper n)
    where helper 0 = []
          helper n = let (q,r) = n `divMod` 2 in r : helper q

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True
intToBool x = error ("intToBool called on the number " ++ show x
                  ++ ". Expected 0 or 1.")
