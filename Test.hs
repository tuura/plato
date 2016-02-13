import System.Exit (exitFailure)

import Tuura.Concept
import Tuura.Concept.Simulation

main :: IO ()
main = do
    testBuffer
    testInverter
    testCElementGateLevel
    testCElementProtocolLevel

testBuffer :: IO ()
testBuffer = do
    putStrLn "=== testBuffer"
    void $ runSimulation bufferSimulation undefined

testInverter :: IO ()
testInverter = do
    putStrLn "=== testInverter"
    void $ runSimulation inverterSimulation undefined

testCElementGateLevel :: IO ()
testCElementGateLevel = do
    putStrLn "=== testCElementGateLevel"
    void $ runSimulation cElementGateLevelSimulation undefined

testCElementProtocolLevel :: IO ()
testCElementProtocolLevel = do
    putStrLn "=== testCElementProtocolLevel"
    void $ runSimulation cElementProtocolLevelSimulation undefined

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = putStrLn $ "OK " ++ show need

data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

shouldBe :: (Eq a, Show a) => a -> a -> Simulation Signal IO ()
shouldBe x y = lift $ assertEq x y

shouldReturn :: (Eq a, Show a) => Simulation Signal IO a -> a -> Simulation Signal IO ()
shouldReturn x y = x >>= (`shouldBe` y)

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

cElementGateLevelSimulation :: Simulation Signal IO ()
cElementGateLevelSimulation = do
    put initialState
    initial circuit initialState `shouldBe` True
    enabledTransitions circuit `shouldReturn` [rise A, rise B]
    fire $ rise A
    enabledTransitions circuit `shouldReturn` [rise B]
    fire $ rise B
    enabledTransitions circuit `shouldReturn` [rise C]
    fire $ rise C
    enabledTransitions circuit `shouldReturn` [fall A, fall B]
  where
    initialState = State $ const False
    circuit      = consistency <> cElement A B C <> inverter C A <> inverter C B

cElementProtocolLevelSimulation :: Simulation Signal IO ()
cElementProtocolLevelSimulation = do
    put initialState
    initial circuit initialState `shouldBe` True
    enabledTransitions circuit `shouldReturn` [rise A, rise B]
    fire $ rise A
    enabledTransitions circuit `shouldReturn` [rise B]
    fire $ rise B
    enabledTransitions circuit `shouldReturn` [rise C]
    fire $ rise C
    enabledTransitions circuit `shouldReturn` [fall A, fall B]
  where
    initialState = State $ const False
    circuit      = consistency <> handshake00 A C <> handshake00 B C
