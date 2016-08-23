module Concept where

import Tuura.ConceptConcat

-- C-element with environment circuit described using signal-level concepts
circuit :: (Eq a) => a -> a -> a -> CircuitConcept a
circuit a b c = interface <> outputRise <> inputFall <> outputFall <> inputRise <> initialState
  where
    interface = inputs [a, b] <> outputs [c]

    outputRise = rise a ~> rise c <> rise b ~> rise c

    inputFall = rise c ~> fall a <> rise c ~> fall b

    outputFall = fall a ~> fall c <> fall b ~> fall c

    inputRise = fall c ~> rise a <> fall c ~> rise b

    initialState = initialise a False <> initialise b False <> initialise c False
