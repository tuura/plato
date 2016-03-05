{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

data SignalType = I | O

data Signal t a where
    Input  :: a -> Signal I a
    Output :: a -> Signal O a

type InputSignal  = Signal I
type OutputSignal = Signal O

data DependencyType = Causality | TimingAssumption | ConcurrencyReduction

type family Dependency a b where
    Dependency I I = TimingAssumption
    Dependency I O = Causality
    Dependency O I = Causality
    Dependency O O = ConcurrencyReduction

(~>) :: (Dependency t1 t2 ~ Causality) => Signal t1 a -> Signal t2 a -> (a, a)
(~>) (Input x) (Output y) = (x, y)
(~>) (Output x) (Input y) = (x, y)
(~>) _ _ = error "impossible" -- This redundant case won't be needed in GHC 8.0

a, b :: InputSignal String
a = Input "a"
b = Input "b"
c :: OutputSignal String
c = Output "c"

p_ac, p_cb :: (String, String)
p_aa = a ~> a
-- p_ab = a ~> b
p_ac = a ~> c
p_cb = c ~> b
-- p_cc = c ~> c

main :: IO ()
main = do
    putStrLn $ show p_ac
    putStrLn $ show p_cb
