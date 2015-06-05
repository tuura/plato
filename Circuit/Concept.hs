module Circuit.Concept (
    CircuitConcept (..),
    consistency,
    causality, (~>), orCausality,
    mutualExclusion, handshake, handshakeWithInitial,
    buffer, inverter, cElement, meElement
    ) where

import Circuit

type CircuitConcept a = Concept (State a) (Transition a)

-- Event-based concepts
consistency :: CircuitConcept a
consistency = excitedConcept before

causality :: Eq a => Transition a -> Transition a -> CircuitConcept a
causality cause effect =
    excitedConcept $ \t -> if t /= effect
                           then const True
                           else after cause .&&. before effect

(~>) :: Eq a => Transition a -> Transition a -> CircuitConcept a
(~>) = causality

orCausality :: Eq a => Transition a -> Transition a -> Transition a -> CircuitConcept a
orCausality cause1 cause2 effect =
    excitedConcept $ \t -> if t /= effect
                           then const True
                           else (after cause1 .||. after cause2) .&&. before effect

mutualExclusion :: Eq a => Transition a -> Transition a -> CircuitConcept a
mutualExclusion a b = toggle a ~> b <> toggle b ~> a

handshake :: Eq a => Transition a -> Transition a -> CircuitConcept a
handshake e f = mconcat [ e        ~> f
                        , f        ~> toggle e
                        , toggle e ~> toggle f
                        , toggle f ~> e ]

handshakeWithInitial :: Eq a => Transition a -> Transition a -> CircuitConcept a
handshakeWithInitial e f = handshake e f <> initialConcept (before e)

-- Singal-based concepts
buffer :: Eq a => a -> a -> CircuitConcept a
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: Eq a => a -> a -> CircuitConcept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

cElement :: Eq a => a -> a -> a -> CircuitConcept a
cElement a b c = buffer a c <> buffer b c

meElement :: Eq a => a -> a -> a -> a -> CircuitConcept a
meElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> mutualExclusion (rise g1) (rise g2)
