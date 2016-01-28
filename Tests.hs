import System.Exit (exitFailure)
import Text.Printf

import Circuit
import Circuit.Concept
import Circuit.Dynamics

main = do
    testBuffer

assertEq want got
    | want /= got = do
        printf "--- FAIL:\nwant: "
        print want
        printf "got:  "
        print got
        exitFailure
    | otherwise = do
        printf "OK "
        print want

data Signal = A | B deriving (Eq, Show, Enum, Bounded)

testBuffer = do
    putStrLn "=== testBuffer"
    assertEq True $ initial circuit initState
    assertEq [rise A] $ enabledTransitions initState circuit
    let afterA = fire (rise A) initState
    assertEq [fall A, rise B] $ enabledTransitions afterA circuit
    let afterB = fire (rise B) afterA
    assertEq [fall A] $ enabledTransitions afterB circuit
    where
        initState = State $ const False
        circuit = consistency <> buffer A B
