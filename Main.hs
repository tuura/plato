import Circuit
import Circuit.Concept
import Circuit.Dynamics

-- Signals
data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

-- Example circuit described using gate-level concepts
example :: CircuitConcept Signal
example = consistency <> cElement A B C <> inverter C A <> inverter C B

-- Example circuit described using protocol-level concepts
example' :: CircuitConcept Signal
example' = consistency <> handshake00 A C <> handshake00 B C

initialState :: State Signal
initialState = State $ const False

-- This program prints the folloing when executed:
-- 000
-- True
-- [A+,B+]
-- 100
-- [B+]
-- 110
-- [C+]
-- 111
-- [A-,B-]
main = do
    print initialState
    print $ initial example initialState
    print $ enabledTransitions initialState example
    let afterA = fire (rise A) initialState
    print afterA
    print $ enabledTransitions afterA example
    let afterB = fire (rise B) afterA
    print afterB
    print $ enabledTransitions afterB example
    let afterC = fire (rise C) afterB
    print afterC
    print $ enabledTransitions afterC example
