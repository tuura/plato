import System.Exit (exitFailure)

import Tuura.Concept.STG
import Tuura.Concept.STG.Translation

main :: IO ()
main = do
    testDotGOutput
    -- testInverter
    -- testCElementGateLevel
    -- testCElementProtocolLevel

testDotGOutput :: IO ()
testDotGOutput = do
    putStrLn "=== testDotGOutput"
    -- result <- runTranslation threeSignalsConcept
    assertEq threeSignalsConcept expected
    -- putStrLn (x `shouldReturn` "Hello")
  where
    expected = ".model out\n"          ++
               ".inputs A\n"           ++
               ".outputs B\n"          ++
               ".internal C\n"         ++
               ".graph\n"              ++
               "A0 A+\n"               ++
               "A+ A1\n"               ++
               "A1 A-\n"               ++
               "A- A0\n"               ++
               "B0 B+\n"               ++
               "B+ B1\n"               ++
               "B1 B-\n"               ++
               "B- B0\n"               ++
               "C0 C+\n"               ++
               "C+ C1\n"               ++
               "C1 C-\n"               ++
               "C- C0\n"               ++
               ".marking {A0 B1 C0}\n" ++
               ".end\n"


-- testInverter :: IO ()
-- testInverter = do
--     putStrLn "=== testInverter"
--     void $ runSimulation inverterSimulation undefined

-- testCElementGateLevel :: IO ()
-- testCElementGateLevel = do
--     putStrLn "=== testCElementGateLevel"
--     void $ runSimulation cElementGateLevelSimulation undefined

-- testCElementProtocolLevel :: IO ()
-- testCElementProtocolLevel = do
--     putStrLn "=== testCElementProtocolLevel"
--     void $ runSimulation cElementProtocolLevelSimulation undefined

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = putStrLn $ "OK " ++ show need

-- data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

-- shouldBe :: (Eq a, Show a) => a -> IO a -> IO ()
-- shouldBe x y = assertEq x y

-- shouldReturn :: (Eq a, Show a) => a -> a -> Bool
-- shouldReturn x y = x >>= (`shouldBe` y)

threeSignalsConcept :: String
threeSignalsConcept = do
    doTranslate signs circuit
  where
    circuit = inputs [a] <> outputs [b] <> internals [c] <> initialise0 [a, c] <> initialise1 [b]
    a = Signal 0
    b = Signal 1
    c = Signal 2
    signs = [a, b, c]

-- bufferSimulation :: Simulation Signal IO ()
-- bufferSimulation = do
--     put initialState
--     initial circuit initialState `shouldBe` True
--     enabledTransitions circuit `shouldReturn` [rise A]
--     fire $ rise A
--     enabledTransitions circuit `shouldReturn` [fall A, rise B]
--     fire $ rise B
--     enabledTransitions circuit `shouldReturn` [fall A]
--   where
--     initialState = State $ const False
--     circuit      = consistency <> buffer A B <> silent C

-- inverterSimulation :: Simulation Signal IO ()
-- inverterSimulation = do
--     put initialState
--     initial circuit initialState `shouldBe` True
--     enabledTransitions circuit `shouldReturn` [rise A]
--     fire $ rise A
--     enabledTransitions circuit `shouldReturn` [fall A, fall B]
--     fire $ fall B
--     enabledTransitions circuit `shouldReturn` [fall A]
--   where
--     initialState = State (== B)
--     circuit      = consistency <> inverter A B <> silent C

-- cElementGateLevelSimulation :: Simulation Signal IO ()
-- cElementGateLevelSimulation = do
--     put initialState
--     initial circuit initialState `shouldBe` True
--     enabledTransitions circuit `shouldReturn` [rise A, rise B]
--     fire $ rise A
--     enabledTransitions circuit `shouldReturn` [rise B]
--     fire $ rise B
--     enabledTransitions circuit `shouldReturn` [rise C]
--     fire $ rise C
--     enabledTransitions circuit `shouldReturn` [fall A, fall B]
--   where
--     initialState = State $ const False
--     circuit      = consistency <> cElement A B C <> inverter C A <> inverter C B

-- cElementProtocolLevelSimulation :: Simulation Signal IO ()
-- cElementProtocolLevelSimulation = do
--     put initialState
--     initial circuit initialState `shouldBe` True
--     enabledTransitions circuit `shouldReturn` [rise A, rise B]
--     fire $ rise A
--     enabledTransitions circuit `shouldReturn` [rise B]
--     fire $ rise B
--     enabledTransitions circuit `shouldReturn` [rise C]
--     fire $ rise C
--     enabledTransitions circuit `shouldReturn` [fall A, fall B]
--   where
--     initialState = State $ const False
--     circuit      = consistency <> handshake00 A C <> handshake00 B C
