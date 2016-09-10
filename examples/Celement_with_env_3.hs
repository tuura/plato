module Concept where

import Tuura.Concept.STG

-- C-element with environment circuit described using protocol-level concepts
circuit a b c = handshake00 a c <> handshake00 b c
				<> inputs [a, b] <> outputs [c]

