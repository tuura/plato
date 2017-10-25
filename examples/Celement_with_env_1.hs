module Concept where

import Tuura.Concept.STG

-- C-element with environment circuit described using signal-level concepts

data Signal = A | B | C deriving (Eq, Enum, Bounded)

system = interface <> outputRise <> inputFall <> outputFall <> inputRise <> initialState
  where
    interface = inputs [A, B] <> outputs [C]

    outputRise = rise A ~> rise C <> rise B ~> rise C

    inputFall = rise C ~> fall A <> rise C ~> fall B

    outputFall = fall A ~> fall C <> fall B ~> fall C

    inputRise = fall C ~> rise A <> fall C ~> rise B

    initialState = initialise A False <> initialise B False <> initialise C False
