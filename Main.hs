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

printState :: Simulation Signal IO ()
printState = (lift . print) =<< get

printEnabledTransitions :: CircuitConcept Signal -> Simulation Signal IO ()
printEnabledTransitions c = do
    ts <- enabledTransitions c
    lift $ print ts

simulation :: CircuitConcept Signal -> Simulation Signal IO ()
simulation c = do
    s <- get
    lift $ print $ initial c s
    printEnabledTransitions c
    fire $ rise A
    printState
    printEnabledTransitions c
    fire $ rise B
    printState
    printEnabledTransitions c
    fire $ rise C
    printState
    printEnabledTransitions c

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
main :: IO ()
main = do
    putStrLn $ "Initial state = " ++ show initialState
    (_, finalState) <- runSimulation (simulation example) initialState
    putStrLn $ "Final state = " ++ show finalState
