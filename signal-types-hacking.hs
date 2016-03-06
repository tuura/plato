{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- Internal (I), output (O) and internal (T) signal types
data SignalTag = I | O | T

data Signal t a where
    Input    :: a -> Signal I a
    Output   :: a -> Signal O a
    Internal :: a -> Signal T a

fromSignal :: Signal t a -> a
fromSignal (Input    a) = a
fromSignal (Output   a) = a
fromSignal (Internal a) = a

type Input    = Signal I
type Output   = Signal O
type Internal = Signal T

data DependencyType = Causality | TimingAssumption | ConcurrencyReduction

type family Dependency (a :: SignalTag) (b :: SignalTag) where
    Dependency I I = TimingAssumption
    Dependency O O = ConcurrencyReduction
    Dependency a b = Causality

(~>) :: (Dependency t1 t2 ~ Causality) => Signal t1 a -> Signal t2 a -> (a, a)
(~>) x y = (fromSignal x, fromSignal y)

a, b :: Input String
a = Input "a"
b = Input "b"
c :: Output String
c = Output "c"
t :: Internal String
t = Internal "t"

p_ac, p_cb, p_tt :: (String, String)
-- p_aa = a ~> a
-- p_ab = a ~> b
p_ac = a ~> c
p_cb = c ~> b
-- p_cc = c ~> c
p_tt = t ~> t

main :: IO ()
main = do
    putStrLn $ show p_ac
    putStrLn $ show p_cb
    putStrLn $ show p_tt
