import Circuit
import Circuit.Concept
import Circuit.Dynamics

-- Signals
data Signal = A | B | C deriving (Eq, Show, Enum, Bounded)

-- oscillatingC :: CircuitConcept Signal
-- oscillatingC = cElement A B C <> inverter C A <> inverter C B

abc = handshakeWithInitial (rise A) (rise B) <> handshakeWithInitial (rise B) (rise C)

initialState :: State Signal
initialState = State $ const False

-- data Signal' = R1 | R2 | G1 | G2 deriving (Eq, Show, Enum, Bounded)

-- oscillatingME = meElement R1 R2 G1 G2 <> inverter G1 R1 <> inverter G2 R2

-- initialState' :: State Signal'
-- initialState' = State $ const False

-- test :: CircuitConcept Signal
-- test = buffer A B <> buffer B C <> inverter C A

-- state1 = fire (rise A) initialState

-- state2 = fire (rise B) state1
-- state3 = fire (rise C) state2

main = do
	print initialState
	print $ initial abc initialState
	print $ enabledTransitions initialState abc
	print $ enabledTransitions (fire (rise A) initialState) abc

	-- print initialState'
	-- print $ enabledTransitions initialState' oscillatingME
	-- let s1 = fire (rise R1) initialState'
	-- print s1
	-- print $ enabledTransitions s1 oscillatingME
	-- let s2 = fire (rise R2) s1
	-- print s2
	-- print $ enabledTransitions s2 oscillatingME
	-- let s3 = fire (rise G1) s2
	-- print s3
	-- print $ enabledTransitions s3 oscillatingME
	-- let s4 = fire (fall R1) s3
	-- print s4
	-- print $ enabledTransitions s4 oscillatingME
	-- let s5 = fire (fall G1) s4
	-- print s5
	-- print $ enabledTransitions s5 oscillatingME
