module Tuura.Plato.Translation where

import Data.Char

data ValidationResult a = Valid | Invalid [a] [a] [a] deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

addErrors :: (Eq a, Show a) => [a] -> [a] -> [a] -> String
addErrors unused incons undef = un ++ ic ++ ud
  where
    un = if (unused /= []) then ("The following signals are not declared as input, output or internal: \n" ++ unlines (map show unused) ++ "\n") else ""
    ic = if (unused /= []) then ("The following signals have inconsistent inital states: \n" ++ unlines (map show incons) ++ "\n") else ""
    ud = if (undef  /= []) then ("The following signals have undefined initial states: \n" ++ unlines (map show undef) ++ "\n") else ""

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct l = sequence l
