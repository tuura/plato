import System.Exit (exitFailure)

import Circuit
import Circuit.Concept
import Circuit.Dynamics

main :: IO ()
main = do
    testBuffer
    testInverter

testBuffer :: IO ()
testBuffer = do
    putStrLn "=== testBuffer"
    void $ runSimulation bufferSimulation undefined

testInverter :: IO ()
testInverter = do
    putStrLn "=== testInverter"
    void $ runSimulation inverterSimulation undefined

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = do
        putStrLn $ "OK " ++ show need

data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

shouldBe :: (Eq a, Show a) => a -> a -> Simulation Signal IO ()
shouldBe x y = lift $ assertEq x y

shouldReturn :: (Eq a, Show a) => Simulation Signal IO a -> a -> Simulation Signal IO ()
shouldReturn x y = (flip shouldBe y) =<< x

bufferSimulation :: Simulation Signal IO ()
bufferSimulation = do
    put initialState
    initial circuit initialState `shouldBe` True
    enabledTransitions circuit `shouldReturn` [rise A]
    fire $ rise A
    enabledTransitions circuit `shouldReturn` [fall A, rise B]
    fire $ rise B
    enabledTransitions circuit `shouldReturn` [fall A]
  where
    initialState = State $ const False
    circuit      = consistency <> buffer A B <> silent C

inverterSimulation :: Simulation Signal IO ()
inverterSimulation = do
    put initialState
    initial circuit initialState `shouldBe` True
    enabledTransitions circuit `shouldReturn` [rise A]
    fire $ rise A
    enabledTransitions circuit `shouldReturn` [fall A, fall B]
    fire $ fall B
    enabledTransitions circuit `shouldReturn` [fall A]
  where
    initialState = State (== B)
    circuit      = consistency <> inverter A B <> silent C
