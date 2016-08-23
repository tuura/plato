module Concept where

import Tuura.ConceptConcat

-- C-element with environment circuit described using protocol-level concepts
circuit :: (Eq a) => a -> a -> a -> CircuitConcept a
circuit a b c = handshake00 a c <> handshake00 b c 
				<> inputs [a, b] <> outputs [c]

