import System.Exit (exitFailure)

import Data.List

import Tuura.Concept.STG
import Tuura.Concept.STG.Translation

main :: IO ()
main = do
    testDotGOutput
    testHandshakeConcept
    testOrGateConcept
    testTwoAndGates
    testTwoDifferentCElements

testDotGOutput :: IO ()
testDotGOutput = do
    putStrLn "=== testDotGOutput"
    assertEq threeSignalsConcept expected
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

testHandshakeConcept :: IO ()
testHandshakeConcept = do
    putStrLn "===testHandshakeConcept"
    assertEq (sort handshakeConcept) (sort expected)
  where
    expected = [([rise a], rise b), ([rise b], fall a), ([fall a], fall b), ([fall b], rise a)]
    a = Signal 0
    b = Signal 1

testOrGateConcept :: IO ()
testOrGateConcept = do
    putStrLn "===testOrGateConcept"
    assertEq (sort orGateConcept) (sort expected)
  where
    expected = [([rise a, rise b], rise c), ([fall a], fall c), ([fall b], fall c)]
    a = Signal 0
    b = Signal 1
    c = Signal 2

testTwoAndGates :: IO ()
testTwoAndGates = do
    putStrLn "===testTwoAndGates"
    assertEq (sort twoAndGatesConcept) (sort expected)
  where
    expected = [([rise a], rise out), ([rise b], rise out), ([rise c], rise out), ([rise d], rise out), ([fall a, fall b], fall out), ([fall c, fall d], fall out)]
    a   = Signal 0
    b   = Signal 1
    c   = Signal 2
    d   = Signal 3
    out = Signal 4

testTwoDifferentCElements :: IO ()
testTwoDifferentCElements = do
    putStrLn "===testTwoDifferentCElements"
    assertEq (sort (arcs cElementConcept1)) (sort (arcs cElementConcept2))

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = putStrLn $ "OK " ++ show need

threeSignalsConcept :: String
threeSignalsConcept = doTranslate signs circuit
  where
    circuit = inputs [a] <> outputs [b] <> internals [c] <> initialise0 [a, c] <> initialise1 [b]
    a = Signal 0
    b = Signal 1
    c = Signal 2
    signs = [a, b, c]

handshakeConcept :: [([Transition Signal], Transition Signal)]
handshakeConcept = arcs circuit
  where
    circuit = inputs[a] <> outputs [b] <> handshake00 a b
    a = Signal 0
    b = Signal 1

orGateConcept :: [([Transition Signal], Transition Signal)]
orGateConcept = arcs circuit
  where
    circuit = inputs [a, b] <> outputs [c] <> initialise0 [a, b, c] <> orGate a b c
    a = Signal 0
    b = Signal 1
    c = Signal 2

twoAndGatesConcept :: [([Transition Signal], Transition Signal)]
twoAndGatesConcept = arcs circuit
  where
    circuit = inputs [a, b, c, d] <> outputs [out] <> initialise0 [a, b, c, d, out] <> andGate a b out <> andGate c d out
    a   = Signal 0
    b   = Signal 1
    c   = Signal 2
    d   = Signal 3
    out = Signal 4

cElementConcept1 :: CircuitConcept Signal
cElementConcept1 = inputs [a, b] <> outputs [c] <> initialise0 [a, b, c] <> buffer a c <> buffer b c
  where
    a = Signal 0
    b = Signal 1
    c = Signal 2

cElementConcept2 :: CircuitConcept Signal
cElementConcept2 = inputs [a, b] <> outputs [c] <> initialise0 [a, b, c] <> cElement a b c
  where
    a = Signal 0
    b = Signal 1
    c = Signal 2
