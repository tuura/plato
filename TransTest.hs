import System.Exit (exitFailure)

import Data.List

import Tuura.Concept.STG
import Tuura.Concept.STG.Translation

import Tuura.Plato.Translate.Translation

main :: IO ()
main = do
    testDotGOutput
    testHandshakeConcept
    testOrGateConcept
    testTwoAndGates
    testTwoDifferentCElements
    testBooleanFunctionConcepts

testDotGOutput :: IO ()
testDotGOutput = do
    putStrLn "=== testDotGOutput"
    assertEq (snd threeSignalsConcept) expected
  where
    threeSignalsConcept = translate (inputs [a] <> outputs [b] <> internals [c]
                       <> initialise0 [a, c] <> initialise1 [b]) signs
    a = Signal 0
    b = Signal 1
    c = Signal 2
    signs = [a, b, c]
    expected = unlines [ ".model out"
                       , ".inputs A"
                       , ".outputs B"
                       , ".internal C"
                       , ".graph"
                       , "A0 A+"
                       , "A+ A1"
                       , "A1 A-"
                       , "A- A0"
                       , "B0 B+"
                       , "B+ B1"
                       , "B1 B-"
                       , "B- B0"
                       , "C0 C+"
                       , "C+ C1"
                       , "C1 C-"
                       , "C- C0"
                       , ".marking {A0 B1 C0}"
                       , ".end"
                       , ""]

testHandshakeConcept :: IO ()
testHandshakeConcept = do
    putStrLn "===testHandshakeConcept"
    assertEq (sort (arcs handshakeConcept)) (sort expected)
  where
    handshakeConcept = inputs[a] <> outputs [b] <> handshake00 a b
    expected = [Causality [rise a] (rise b), Causality [rise b] (fall a),
                Causality [fall a] (fall b), Causality [fall b] (rise a)]
    [a, b] = map Signal [0 .. 1]

testOrGateConcept :: IO ()
testOrGateConcept = do
    putStrLn "===testOrGateConcept"
    assertEq (sort (arcs orGateConcept)) (sort expected)
  where
    orGateConcept = inputs [a, b] <> outputs [c]
                 <> initialise0 [a, b, c] <> orGate a b c
    expected = [Causality [rise a, rise b] (rise c),
                Causality [fall a] (fall c), Causality [fall b] (fall c)]
    [a, b, c] = map Signal [0..2]

testTwoAndGates :: IO ()
testTwoAndGates = do
    putStrLn "===testTwoAndGates"
    assertEq (sort (arcs twoAndGatesConcept)) (sort expected)
  where
    twoAndGatesConcept = inputs [a, b, c, d] <> outputs [out]
                      <> initialise0 [a, b, c, d, out] <> andGate a b out
                      <> andGate c d out
    expected = [Causality [rise a] (rise out), Causality [rise b] (rise out),
                Causality [rise c] (rise out), Causality [rise d] (rise out),
                Causality [fall a, fall b] (fall out),
                Causality [fall c, fall d] (fall out)]
    [a, b, c, d, out] = map Signal [0..4]

testTwoDifferentCElements :: IO ()
testTwoDifferentCElements = do
    putStrLn "===testTwoDifferentCElements"
    assertEq (sort (arcs cElementConcept))
             (sort (arcs cElementConceptFromBuffers))
  where
    cElementConcept = inputs [a, b] <> outputs [c]
                   <> initialise0 [a, b, c] <> cElement a b c
    cElementConceptFromBuffers = inputs [a, b] <> outputs [c]
                              <> initialise0 [a, b, c] <> buffer a c
                              <> buffer b c
    [a, b, c] = map Signal [0..2]

testBooleanFunctionConcepts :: IO ()
testBooleanFunctionConcepts = do
    putStrLn "===testBooleanFunctionConcepts"
    assertEq (sort (arcs andGateFunction))
             (sort (arcs andGateConcept))
  where
    andGateFunction = combinationalGate (And (Var x) (Var y)) z
    andGateConcept = andGate x y z
    [x, y, z] = map Signal [0..2]

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = putStrLn $ "OK " ++ show need
